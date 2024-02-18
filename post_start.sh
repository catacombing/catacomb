#!/bin/sh

# This configuration file describes a minimal example for a usable mobile
# experience.
#
# The following applications should be installed or replaced:
# - tremor: Used for vibration effects (https://github.com/catacombing/tremor)
# - epitaph: Panel (https://github.com/catacombing/epitaph)
# - tzompantli: Application launcher (https://github.com/catacombing/tzompantli)
# - squeekboard: Virtual Keyboard (https://gitlab.gnome.org/World/Phosh/squeekboard)

# Exit on error
set -e

# Key Bindings

# Ignore power-button press, to allow binding to it
systemd-inhibit --what handle-power-key sleep infinity &

## Launch `tzompantli` when holding the power button
catacomb msg bind-key --on-press '*' XF86PowerOff bash -c \
    "sleep 0 0 0 0 0 0 0 0.5 && \
        if [ \"\$(catacomb msg dpms)\" == \"on\" ]; then \
            (tremor 100 0 1; tzompantli) \
        fi"

## Turn off display when pressing the power button
catacomb msg bind-key '*' XF86PowerOff bash -c \
    "if pkill -xf -9 \"sleep 0 0 0 0 0 0 0 0.5\"; then \
        if [ \"\$(catacomb msg dpms)\" == \"on\" ]; then \
            catacomb msg dpms off; \
        else \
            catacomb msg dpms on; \
        fi \
    fi"

# Per-application scale overrides
catacomb msg scale --app-id firefox 1.5

# Spawn background apps
squeekboard &
epitaph &

# Turn off display after 3 minutes, suspend 30s later
swayidle -w \
    timeout 180 'catacomb msg dpms off' \
        resume 'catacomb msg dpms on' \
    timeout 210 'systemctl suspend' \
        after-resume 'catacomb msg dpms on' &

# Wait for completion
wait
