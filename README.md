# Catacomb - A Wayland Mobile Compositor

## About

Catacomb is a Wayland compositor for Linux smartphones. It aims to provide
responsive window management without cutting down on useful features.

## Screenshots

<p align="center">
  <img src="https://user-images.githubusercontent.com/8886672/210189210-6a70de47-1bfe-46e0-b4e7-e4921a9c5ff5.png" width="30%"/>
  <img src="https://user-images.githubusercontent.com/8886672/213074577-28b081dc-d614-443e-beb1-8681e060595c.png" width="30%"/>
  <img src="https://user-images.githubusercontent.com/8886672/210189206-3d9d738f-dd60-47bb-99ab-7a6450be9da1.png" width="30%"/>
</p>

## Configuration

Catacomb is configured through IPC using `catacomb msg`.

Documentation for available configuration options is available under `catacomb
msg -h`.

For persistent configuration or to launch applications on startup, Catacomb
automatically loads `$XDG_CONFIG_HOME/catacomb/post_start.sh` (or
`~/.config/catacomb/post_start.sh` if `XDG_CONFIG_HOME` is not defined).

You can find an example configuration [here](./post_start.sh).

## Controls

| Open application overview                   | Minimize everything              | Close applications         |
| ------------------------------------------- | -------------------------------- | -------------------------- |
| ![open overview](./docs/enter_overview.mp4) | ![minimize](./docs/minimize.mp4) | ![close](./docs/close.mp4) |

| Double-tap to cycle        | Tiling                       |
| -------------------------- | ---------------------------- |
| ![cycle](./docs/cycle.mp4) | <video src ="./docs/tiling.mp4"/>) |
