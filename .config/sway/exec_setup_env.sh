#!/bin/bash
#
# Setup systemd environment, but only on the "main" sway.
#
# See: /etc/sway/config.d/50-systemd-user.conf
#

case "$WAYLAND_DISPLAY"
in
    wayland-1)
        ;;
    wayland-*)
        echo "exec_setup_env: wont setup environment on secondary wayland '$WAYLAND_DISPLAY'" >&2
        exit 0
        ;;
    *)
        echo "exec_setup_env: error: unexpected WAYLAND_DISPLAY=$WAYLAND_DISPLAY" >&2
        exit 1
        ;;
esac

# /etc/sway/config.d/50-systemd-user.conf

systemctl --user set-environment XDG_CURRENT_DESKTOP=sway
systemctl --user import-environment DISPLAY \
          SWAYSOCK \
          WAYLAND_DISPLAY \
          XDG_CURRENT_DESKTOP

hash dbus-update-activation-environment 2>/dev/null && \
    dbus-update-activation-environment --systemd DISPLAY \
                                       SWAYSOCK \
                                       XDG_CURRENT_DESKTOP=sway \
                                       WAYLAND_DISPLAY
