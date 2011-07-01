#!/usr/bin/python3

import glob
import random
import os

from re import escape

wall_dir='/home/jo/.i3/wallpapers/'

wallpapers = glob.glob(wall_dir + '*')

print(wallpapers);

os.system('feh --bg-fill ' + escape(random.choice(wallpapers)))
