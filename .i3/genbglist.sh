#!/bin/bash

find wallpapers -type f | sed 's#^#~/.i3/#g' | tee ./bg.list
