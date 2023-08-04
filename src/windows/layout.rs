//! Window workspace layout.

use std::cell::{Ref, RefCell, RefMut};
use std::cmp::Ordering;
use std::mem;
use std::rc::{Rc, Weak};
use std::sync::atomic::{self, AtomicU64};

use smithay::utils::{Logical, Point};
use smithay::wayland::shell::xdg::ToplevelSurface;

use crate::drawing::CatacombElement;
use crate::windows::surface::InputSurfaceKind;
use crate::windows::{self, InputSurface, Output, Window};

/// Default layout as const for borrowing purposes.
const DEFAULT_LAYOUT: Layout = Layout { primary: None, secondary: None, id: LayoutId(0) };

/// Counter for layout IDs.
static NEXT_LAYOUT_ID: AtomicU64 = AtomicU64::new(1);

/// Active workspaces.
#[derive(Debug, Default)]
pub struct Layouts {
    pub focus: Option<Weak<RefCell<Window>>>,

    /// Layout calltree.
    ///
    /// This stores the layout from which the active layout was created and its
    /// respective ancestors. Used to restore the previous layout after the
    /// window is closed.
    parent_layouts: Vec<LayoutId>,

    transactions: Vec<Transaction>,
    active_layout: Option<usize>,
    layouts: Vec<Layout>,
}

impl Layouts {
    /// Get layout at the specified offset.
    pub fn get(&self, index: usize) -> Option<&Layout> {
        self.layouts.get(index)
    }

    /// Get the currently visible layout.
    pub fn active(&self) -> &Layout {
        self.active_layout.and_then(|i| self.layouts.get(i)).unwrap_or(&DEFAULT_LAYOUT)
    }

    /// Get pending visible layout.
    ///
    /// This will report the transaction's layout and fall back to
    /// `Self::active` if no transaction is active.
    pub fn pending_active(&self) -> Layout {
        let mut layout = self.active().clone();

        for transaction in &self.transactions {
            match transaction {
                Transaction::Active(Some(index)) => {
                    if let Some(new_layout) = self.layouts.get(*index) {
                        layout = new_layout.clone();
                    }
                },
                Transaction::Active(None) => layout = DEFAULT_LAYOUT,
                Transaction::Primary(position) => {
                    if let Some(new_layout) = self.layouts.get(position.index) {
                        if position.secondary {
                            layout.primary = new_layout.secondary.clone();
                        } else {
                            layout.primary = new_layout.primary.clone();
                        }
                    }
                },
                Transaction::Secondary(position) => {
                    if let Some(new_layout) = self.layouts.get(position.index) {
                        if position.secondary {
                            layout.secondary = new_layout.secondary.clone();
                        } else {
                            layout.secondary = new_layout.primary.clone();
                        }
                    }
                },
            }
        }

        layout
    }

    /// Overview offset of the active layout.
    pub fn active_offset(&self) -> f64 {
        -(self.active_layout.unwrap_or(0) as f64)
    }

    /// Create and activate a new layout using the desired primary window.
    pub fn create(&mut self, output: &Output, primary: Rc<RefCell<Window>>) {
        // Update ancestors.
        let active_layout = self.active();
        if active_layout.id != DEFAULT_LAYOUT.id {
            self.parent_layouts.push(active_layout.id);
        }

        // Issue resize for the new window.
        let rectangle = output.primary_rectangle(false);
        primary.borrow_mut().set_dimensions(output.scale(), rectangle);

        // Create layout for the new window.
        self.layouts.push(Layout::new(primary));

        // Stage switch to the new layout.
        let position = LayoutPosition::new(self.layouts.len() - 1, false);
        self.set_active(output, Some(position), false);
    }

    /// Switch the active layout.
    pub fn set_active(
        &mut self,
        output: &Output,
        layout_position: Option<LayoutPosition>,
        clear_parents: bool,
    ) {
        let layout_index = layout_position.map(|position| position.index);

        self.focus = None;
        if let Some(position) = layout_position {
            // Send enter and update focus for new layout's windows.
            let layout = self.layouts.get(position.index).unwrap_or(&DEFAULT_LAYOUT);
            let window = if position.secondary { &layout.secondary } else { &layout.primary };
            if let Some(window) = window {
                self.focus = Some(Rc::downgrade(window));
                window.borrow_mut().enter(output);
            }

            // Ensure layout's windows have the correct size.
            layout.resize(output);
        }

        // Clear layout history.
        if clear_parents {
            self.parent_layouts.clear();
        }

        self.add_transaction(Transaction::Active(layout_index));
    }

    /// Cycle through window layouts.
    ///
    /// This will switch the layout to the one `n` layouts away from it.
    pub fn cycle_active(&mut self, output: &Output, mut n: isize) {
        // Ignore cycle without any windows.
        let layout_count = self.layouts.len();
        if layout_count == 0 {
            return;
        }

        let active_index = match self.active_layout {
            Some(active_layout) => active_layout,
            // Use first "step" to cycle from no active layout to index 0.
            None => {
                n -= n.signum();
                0
            },
        };

        let target_layout = (active_index as isize + n).rem_euclid(layout_count as isize);
        let position = LayoutPosition::new(target_layout as usize, false);
        self.set_active(output, Some(position), true);
    }

    /// Update the active layout's primary window.
    pub fn set_primary(&mut self, output: &Output, position: LayoutPosition) {
        let layout = match self.layouts.get(position.index) {
            Some(layout) => layout,
            None => return,
        };

        // Perform simple layout swap when no resize is necessary.
        if layout.secondary.is_none() {
            self.set_active(output, Some(position), true);
            return;
        }

        // Resize both windows to fullscreen, since secondary will be split off.
        for window in layout.windows() {
            let rectangle = output.primary_rectangle(false);
            window.borrow_mut().set_dimensions(output.scale(), rectangle);
        }

        // Send enter event for new primary.
        let window = if position.secondary { &layout.secondary } else { &layout.primary };
        if let Some(window) = window {
            self.focus = Some(Rc::downgrade(window));
            window.borrow_mut().enter(output);
        }

        self.add_transaction(Transaction::Primary(position));
    }

    /// Update the active layout's secondary window.
    pub fn set_secondary(&mut self, output: &Output, position: LayoutPosition) {
        let layout = match self.layouts.get(position.index) {
            Some(layout) => layout,
            None => return,
        };

        let active = self.active();
        match active.primary.as_ref() {
            // Resize primary if present.
            Some(primary) => {
                let rectangle = output.primary_rectangle(true);
                primary.borrow_mut().set_dimensions(output.scale(), rectangle);
            },
            // Block setting secondary without primary present.
            None => {
                self.set_primary(output, position);
                return;
            },
        }

        // Resize old secondary since it will get booted.
        if let Some(secondary) = active.secondary.as_ref() {
            let rectangle = output.primary_rectangle(false);
            secondary.borrow_mut().set_dimensions(output.scale(), rectangle);
        }

        // Resize new secondary if it was primary before.
        if let Some(primary) = layout.primary.as_ref().filter(|_| !position.secondary) {
            let rectangle = output.secondary_rectangle();
            primary.borrow_mut().set_dimensions(output.scale(), rectangle);
        }

        // Resize old layout's sibling since we split the layout up.
        let sibling = if position.secondary { &layout.primary } else { &layout.secondary };
        if let Some(sibling) = sibling {
            let rectangle = output.primary_rectangle(false);
            sibling.borrow_mut().set_dimensions(output.scale(), rectangle);
        }

        // Send enter event for new secondary.
        let window = if position.secondary { &layout.secondary } else { &layout.primary };
        if let Some(window) = window {
            self.focus = Some(Rc::downgrade(window));
            window.borrow_mut().enter(output);
        }

        self.add_transaction(Transaction::Secondary(position));
    }

    /// Stage a dead window for reaping.
    pub fn reap(&self, output: &Output, surface: &ToplevelSurface) {
        // Ensure window is reaped even if no resize is required.
        windows::start_transaction();

        for layout in &self.layouts {
            let primary = layout.primary.as_deref().map(RefCell::borrow_mut);
            let secondary = layout.secondary.as_deref().map(RefCell::borrow_mut);

            // Determine window which might need resizing.
            let growing_window = if primary.as_ref().map_or(false, |win| &win.surface == surface) {
                secondary
            } else if secondary.as_ref().map_or(false, |win| &win.surface == surface) {
                primary
            } else {
                continue;
            };

            // Resize window to fullscreen if present.
            if let Some(mut window) = growing_window {
                let rectangle = output.primary_rectangle(false);
                window.set_dimensions(output.scale(), rectangle);
            }

            // Quit as soon as any matching surface was found.
            break;
        }
    }

    /// Apply all pending transaction updates.
    pub fn apply_transaction(&mut self, output: &Output) {
        // Apply transactional layout changes.
        for i in 0..self.transactions.len() {
            match self.transactions[i] {
                Transaction::Active(layout) => {
                    self.apply_set_active_transaction(output, layout);
                },
                Transaction::Primary(position) => {
                    self.apply_primary_transaction(output, position);
                },
                Transaction::Secondary(position) => {
                    self.apply_secondary_transaction(output, position);
                },
            }
        }
        self.transactions.clear();

        // Reap dead windows and apply window transactions.
        let mut clear_active = false;
        let mut index = 0;
        self.layouts.retain_mut(|layout| {
            // Update secondary window transaction and liveliness.
            if let Some(secondary) = layout.secondary.as_ref() {
                let mut secondary = secondary.borrow_mut();
                if secondary.alive() {
                    secondary.apply_transaction();
                } else {
                    drop(secondary);
                    layout.secondary = None;
                }
            }

            // Update primary window transaction and liveliness.
            if let Some(primary) = layout.primary.as_ref() {
                let mut primary = primary.borrow_mut();
                if primary.alive() {
                    primary.apply_transaction();
                } else {
                    drop(primary);
                    layout.primary = layout.secondary.take();
                }
            }

            // Remove the layout when all windows have died.
            let retain = layout.primary.is_some() || layout.secondary.is_some();

            // Adjust active layout index.
            match Some(index).cmp(&self.active_layout) {
                // Decrement index when layout before it is removed.
                Ordering::Less if !retain => {
                    self.active_layout =
                        self.active_layout.and_then(|active| active.checked_sub(1));
                },
                Ordering::Equal if !retain => clear_active = true,
                _ => (),
            }

            index += 1;

            retain
        });

        if clear_active {
            // Set active layout to parent or clear it.
            let parent = self.parent_layouts.pop();
            self.active_layout = self.layouts.iter().position(|layout| Some(layout.id) == parent);

            // Abandon history.
            if self.active_layout.is_none() {
                self.parent_layouts.clear();
            }
        }
    }

    /// Apply a layout switch transaction.
    fn apply_set_active_transaction(&mut self, output: &Output, layout: Option<usize>) {
        // Skip no-ops.
        if layout == self.active_layout {
            return;
        }

        // Send leave event to old layout's windows.
        self.send_active_leave(output);

        self.active_layout = layout;
    }

    /// Apply a primary layout change transaction.
    fn apply_primary_transaction(&mut self, output: &Output, position: LayoutPosition) {
        let layout = match self.layouts.get_mut(position.index) {
            Some(layout) => layout,
            None => return,
        };

        // Ensure transaction was not invalidated by previous transaction.
        if (position.secondary && layout.secondary.is_none())
            || (!position.secondary && layout.primary.is_none())
        {
            return;
        }

        // Split secondary into new layout.
        if let Some(window) = layout.secondary.take() {
            self.layouts.push(Layout::new(window));
        }

        // Send leave event to old layout's windows.
        self.send_active_leave(output);

        // Switch to layout with new primary window.
        if position.secondary {
            self.active_layout = Some(self.layouts.len() - 1);
        } else {
            self.active_layout = Some(position.index);
        }
    }

    /// Apply a secondary layout change transaction.
    fn apply_secondary_transaction(&mut self, output: &Output, position: LayoutPosition) {
        let layout = match self.layouts.get_mut(position.index) {
            Some(layout) => layout,
            None => return,
        };

        // Ensure transaction was not invalidated by previous transaction.
        if (position.secondary && layout.secondary.is_none())
            || (!position.secondary && layout.primary.is_none())
        {
            return;
        }

        // Move secondary to primary if we're taking the primary away.
        if !position.secondary {
            // Send leave for old secondary.
            if let Some(secondary) = &layout.secondary {
                secondary.borrow_mut().leave(output);
            }

            mem::swap(&mut layout.primary, &mut layout.secondary);
        }

        // Remove new secondary from old layout.
        let secondary = layout.secondary.take();
        let has_primary = layout.primary.is_some();

        let active_layout = self.active_layout.and_then(|i| self.layouts.get_mut(i));
        let active_layout = match active_layout {
            Some(active_layout) => active_layout,
            // Switch to existing layout when none is active and it is on its own already.
            None if !has_primary => {
                self.active_layout = Some(position.index);
                return;
            },
            // Create new layout when none is currently active.
            None => {
                self.layouts.push(DEFAULT_LAYOUT);
                let index = self.layouts.len() - 1;
                self.active_layout = Some(index);
                &mut self.layouts[index]
            },
        };

        // Replace the active layout's secondary window.
        let old_secondary = mem::replace(&mut active_layout.secondary, secondary);

        // Move active layout's old secondary to its own layout.
        if let Some(window) = old_secondary {
            self.layouts.push(Layout::new(window));
        }
    }

    /// Add transactional update.
    fn add_transaction(&mut self, transaction: Transaction) {
        windows::start_transaction();
        self.transactions.push(transaction);
    }

    /// Send leave event to active layout's windows.
    fn send_active_leave(&mut self, output: &Output) {
        self.with_visible_mut(|window| window.leave(output));
    }

    /// Execute a function for all visible windows.
    pub fn with_visible<F: FnMut(&Window)>(&self, mut fun: F) {
        let layout = self.active();
        for window in layout.windows() {
            fun(&window.borrow());
        }
    }

    /// Execute a function for all visible windows mutably.
    pub fn with_visible_mut<F: FnMut(&mut Window)>(&mut self, mut fun: F) {
        let layout = self.active();
        for window in layout.windows() {
            fun(&mut window.borrow_mut());
        }
    }

    /// Add all visible windows' textures to the supplied buffer.
    pub fn textures(&self, textures: &mut Vec<CatacombElement>, scale: f64) {
        let layout = self.active();

        if let Some(secondary) = layout.secondary().map(|window| window.borrow()) {
            secondary.textures(textures, scale, None, None);
        }

        if let Some(primary) = layout.primary().map(|window| window.borrow()) {
            primary.textures(textures, scale, None, None);
        }
    }

    /// Get an iterator over all windows.
    pub fn windows(&self) -> impl Iterator<Item = Ref<Window>> {
        self.layouts.iter().flat_map(|layout| layout.windows()).map(|window| window.borrow())
    }

    /// Get an iterator over all windows.
    pub fn windows_mut(&mut self) -> impl Iterator<Item = RefMut<Window>> {
        self.layouts.iter().flat_map(|layout| layout.windows()).map(|window| window.borrow_mut())
    }

    /// Get overview layout position of a window.
    pub fn position(&self, window: &Rc<RefCell<Window>>) -> Option<LayoutPosition> {
        for (i, layout) in self.layouts.iter().enumerate() {
            match (&layout.primary, &layout.secondary) {
                (Some(primary), _) if Rc::ptr_eq(primary, window) => {
                    return Some(LayoutPosition::new(i, false))
                },
                (_, Some(secondary)) if Rc::ptr_eq(secondary, window) => {
                    return Some(LayoutPosition::new(i, true))
                },
                _ => (),
            }
        }
        None
    }

    /// Convert overview layout position to winow.
    pub fn window_at(&self, position: LayoutPosition) -> Option<&Rc<RefCell<Window>>> {
        self.layouts.get(position.index).and_then(|layout| {
            if position.secondary {
                layout.secondary.as_ref()
            } else {
                layout.primary.as_ref()
            }
        })
    }

    /// Find the window for the given toplevel surface.
    pub fn find_window(&self, surface: &ToplevelSurface) -> Option<&Rc<RefCell<Window>>> {
        self.layouts
            .iter()
            .flat_map(|layout| layout.windows())
            .find(|window| &window.borrow().surface == surface)
    }

    /// Touch and return surface at the specified location.
    pub fn touch_surface_at(
        &mut self,
        output_scale: f64,
        position: Point<f64, Logical>,
    ) -> Option<InputSurface> {
        let active_layout = self.active_layout.and_then(|index| self.layouts.get(index))?;

        for window in active_layout.windows() {
            let window_ref = window.borrow();
            if window_ref.contains(output_scale, position) {
                let mut surface = window_ref.surface_at(output_scale, position)?;
                let app_id = window_ref.app_id.clone();
                let window = Rc::downgrade(window);
                surface.toplevel = Some(InputSurfaceKind::Layout((window, app_id)));
                return Some(surface);
            }
        }

        None
    }

    /// Get all known layouts.
    pub fn layouts(&self) -> &[Layout] {
        &self.layouts
    }

    /// Check if there are any layouts.
    pub fn is_empty(&self) -> bool {
        self.layouts.is_empty()
    }

    /// Layout count.
    pub fn len(&self) -> usize {
        self.layouts.len()
    }
}

/// Workspace window layout.
#[derive(Clone, Debug)]
pub struct Layout {
    primary: Option<Rc<RefCell<Window>>>,
    secondary: Option<Rc<RefCell<Window>>>,
    id: LayoutId,
}

impl Layout {
    fn new(primary: Rc<RefCell<Window>>) -> Self {
        let id = LayoutId(NEXT_LAYOUT_ID.fetch_add(1, atomic::Ordering::Relaxed));
        Self { id, primary: Some(primary), secondary: None }
    }

    /// Get layout's primary window.
    pub fn primary(&self) -> Option<&Rc<RefCell<Window>>> {
        self.primary.as_ref()
    }

    /// Get layout's secondary window.
    pub fn secondary(&self) -> Option<&Rc<RefCell<Window>>> {
        self.secondary.as_ref()
    }

    /// Get an iterator over all windows in this layout.
    pub fn windows(&self) -> impl Iterator<Item = &Rc<RefCell<Window>>> {
        self.primary.iter().chain(&self.secondary)
    }

    /// Get number of visible windows.
    pub fn window_count(&self) -> usize {
        let primary_count = if self.primary.is_some() { 1 } else { 0 };
        let secondary_count = if self.secondary.is_some() { 1 } else { 0 };
        primary_count + secondary_count
    }

    /// Check if the layout contains any windows.
    pub fn is_empty(&self) -> bool {
        self.primary.is_none() && self.secondary.is_none()
    }

    /// Resize all windows in this layout to their expected size.
    pub fn resize(&self, output: &Output) {
        let primary = self.primary.as_deref().map(RefCell::borrow_mut);
        let secondary = self.secondary.as_deref().map(RefCell::borrow_mut);

        if let Some(mut primary) = primary {
            let secondary_alive = secondary.as_ref().map_or(false, |window| window.alive());
            let rectangle = output.primary_rectangle(secondary_alive);
            primary.set_dimensions(output.scale(), rectangle);
        }

        if let Some(mut secondary) = secondary {
            let rectangle = output.secondary_rectangle();
            secondary.set_dimensions(output.scale(), rectangle);
        }
    }
}

/// Unique layout ID.
#[derive(Copy, Clone, PartialEq, Eq, Debug, Default)]
struct LayoutId(u64);

/// Transactional layout change.
#[derive(Debug)]
enum Transaction {
    Active(Option<usize>),
    Primary(LayoutPosition),
    Secondary(LayoutPosition),
}

/// Reference to a specific window in a layout.
#[derive(Copy, Clone, Debug)]
pub struct LayoutPosition {
    pub index: usize,
    pub secondary: bool,
}

impl LayoutPosition {
    pub fn new(index: usize, secondary: bool) -> Self {
        Self { index, secondary }
    }
}
