# Changelog

All notable changes are documented in this file.
The sections should follow the order `Packaging`, `Added`, `Changed`, `Fixed` and `Removed`.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## Unreleased

### Fixed

- Orientation tracking for accelerometers with `mount_matrix` udev attribute
- XDG popup support for layer shell windows

## 1.0.6 - 2025-10-19

### Changed

- Subprocess STDOUT and STDERR is now forwarded

### Fixed

- Individual clients causing consistent transaction timeouts

## 1.0.5 - 2025-09-19

### Added

- Multi-button keybinding support
- Screenshot binding in default config

## 1.0.4 - 2025-09-10

### Added

- IPC message `keyboard-config` for changing keyboard layout and options

### Fixed

- Missing VBlanks on low-power devices due to high GPU render times

## 1.0.3 - 2025-08-12

### Fixed

- Damage tracking for viewporter clients
- Rendering on devices without instancing support
- No applications getting scanned out directly

## 1.0.2 - 2025-07-02

### Fixed

- Invalid gsettings command in default config

## 1.0.1 - 2025-07-02

### Fixed

- Squeekboard not opening with default config

## 1.0.0 - 2025-07-01

Initial Release.
