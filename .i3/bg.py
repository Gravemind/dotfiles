#!/usr/bin/python3

import glob
import random
import os

from re import escape

path_home = os.environ['HOME']
bgs_file = open(path_home + '/.i3/bg.list', 'r');
wallpapers = []
for line in bgs_file:
    line = line.replace('~', path_home)
    if line[-1] == '\n':
        line = line[0:-1]
    if os.path.exists(line):
        wallpapers[0:0] = [line]
    else:
        files = glob.glob(line)
        if len(files) == 0:
            print('bg.py: Warning: no files for: '+line)
        else:
            wallpapers[0:0] = files

bg = random.choice(wallpapers)
print(bg)
os.system('feh --bg-fill ' + escape(bg))
