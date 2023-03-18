//! Control device rumble.

use std::fs::File;
use std::io::Write;
use std::os::unix::io::AsRawFd;
use std::time::Duration;
use std::{fmt, mem, slice};

use nix::{ioctl_write_int, ioctl_write_ptr};
use smithay::reexports::calloop::timer::{TimeoutAction, Timer};
use smithay::reexports::calloop::LoopHandle;

use crate::catacomb::Catacomb;

/// Force-feedback device path.
const DEVICE_PATH: &str = "/dev/input/by-path/platform-vibrator-event";

/// Force-feedback event type constant.
/// <https://github.com/torvalds/linux/blob/9f4211bf7f811b653aa6acfb9aea38222436a458/include/uapi/linux/input-event-codes.h#L47>
const EV_FF: u16 = 0x15;

/// Force-feedback interface.
pub struct Vibrator {
    device: Option<File>,
    event_loop: LoopHandle<'static, Catacomb>,
}

impl Vibrator {
    pub fn new(event_loop: LoopHandle<'static, Catacomb>) -> Self {
        Self { event_loop, device: File::options().append(true).open(DEVICE_PATH).ok() }
    }

    /// Play a rumble effect.
    pub fn vibrate(&mut self, length: u16, interval: u16, count: u16) {
        unsafe {
            if let Err(err) = self.play_rumble(length, interval, count) {
                eprintln!("{err}");
            }
        }
    }

    /// Stop vibration and remove effect from device.
    pub fn stop(&mut self, effect_id: u64) {
        // Ignore without rumble device access.
        let fd = match &self.device {
            Some(device) => device.as_raw_fd(),
            None => return,
        };

        if let Err(err) = unsafe { remove_effect(fd, effect_id) } {
            eprintln!("Failed to remove rumble effect: {err}");
        }
    }

    /// Play a rumble effect.
    ///
    /// Unsafe wrapper for the purpose of error handling.
    /// Use [`Self::vibrate`] instead.
    unsafe fn play_rumble(&mut self, length: u16, interval: u16, count: u16) -> Result<(), String> {
        // Ignore without rumble device access.
        let device = match &mut self.device {
            Some(device) => device,
            None => return Ok(()),
        };

        let mut effect = Effect {
            effect_type: 0x50,
            id: -1,
            direction: 0,
            trigger: Trigger { interval: 0, button: 0 },
            replay: Replay { length, delay: interval },
            data: EffectData { rumble: Rumble { strong: u16::MAX, weak: 0 } },
        };

        // Upload effect to the device.
        match upload_effect(device.as_raw_fd(), &mut effect as *const _) {
            Err(err) => return Err(format!("Failed to upload rumble effect: {err}")),
            Ok(_) if effect.id < 0 => return Err(format!("Invalid rumble effect ID: {effect:?}")),
            Ok(_) => (),
        }

        // Play effect `count` times.
        let play = libc::input_event {
            time: libc::timeval { tv_sec: 0, tv_usec: 0 },
            code: effect.id as u16,
            value: count as i32,
            type_: EV_FF,
        };
        let play_ptr = (&play as *const libc::input_event).cast();
        let play_size = mem::size_of::<libc::input_event>();
        let play_data = slice::from_raw_parts(play_ptr, play_size);
        device.write(play_data).map_err(|err| format!("Failed to submit rumble event: {err}"))?;

        // Schedule effect removal.
        let duration = Duration::from_millis(((length + interval) * count) as u64);
        self.event_loop
            .insert_source(Timer::from_duration(duration), move |_, _, catacomb| {
                catacomb.vibrator.stop(effect.id as u64);
                TimeoutAction::Drop
            })
            .map_err(|err| format!("Failed to remove rumble effect: {err}"))?;

        Ok(())
    }
}

ioctl_write_ptr!(upload_effect, b'E', 0x80, Effect);
ioctl_write_int!(remove_effect, b'E', 0x81);

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct Effect {
    effect_type: u16,
    id: i16,
    direction: u16,
    trigger: Trigger,
    replay: Replay,
    data: EffectData,
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
struct Trigger {
    interval: u16,
    button: u16,
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
struct Replay {
    length: u16,
    delay: u16,
}

#[repr(C)]
#[derive(Copy, Clone)]
union EffectData {
    rumble: Rumble,
    #[cfg(target_pointer_width = "64")]
    padding: [u64; 4],
    #[cfg(target_pointer_width = "32")]
    padding: [u32; 7],
}

impl fmt::Debug for EffectData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe { self.padding.fmt(f) }
    }
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
struct Rumble {
    strong: u16,
    weak: u16,
}
