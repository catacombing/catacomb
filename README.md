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

## Polkit

Catacomb uses logind's DBus API to automatically suspend the system when the
power button is pressed. To allow users of the group `wheel` to suspend the
system, the following polkit rule can be added:

> /etc/polkit-1/rules.d/20-logind.rules

```
polkit.addRule(function(action, subject) {
    // Allow wheel users to suspend.
    if (action.id == "org.freedesktop.login1.suspend" && subject.isInGroup("wheel")) {
        return "yes";
    }

    // Allow wheel users to block power button handling.
    if (action.id == "org.freedesktop.login1.inhibit-handle-power-key" && subject.isInGroup("wheel")) {
        return "yes";
    }
});
```
