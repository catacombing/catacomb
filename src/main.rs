use std::env;

mod catacomb;
mod config;
mod daemon;
mod drawing;
mod geometry;
mod input;
mod layer;
mod orientation;
mod output;
mod overview;
mod shell;
mod udev;
mod window;
#[cfg(feature = "winit")]
mod winit;

fn main() {
    // Do not turn children into zombies.
    unsafe { libc::signal(libc::SIGCHLD, libc::SIG_IGN) };

    if env::var_os("DISPLAY").is_none() && env::var_os("WAYLAND_DISPLAY").is_none() {
        udev::run();
    } else {
        #[cfg(feature = "winit")]
        winit::run();
    }
}
