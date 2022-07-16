#!/bin/bash

clipman pick -t rofi && {
    wl-paste -n | wl-copy --primary
}
