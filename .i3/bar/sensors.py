#!/usr/bin/python3

import os
import subprocess

class Sensors:
    temp = {}
    command = ''

    def __init__(self, command = 'sensors'):
        self.command = command

    def update(self):
        self.temp = {}
        out = subprocess.getoutput(self.command)
        secteur = 'all'
        for line in out.splitlines():
            line = line.strip()
            if len(line) == 0:
                continue
            colon = line.find(':')
            if colon == -1:
                secteur = line
                self.temp[secteur] = {}
            else:
                key = line[:colon]
                degree = line.find('°')
                if degree != -1:
                    self.temp[secteur][key] = int(float(
                            line[ line[:degree].rfind(' ')
                                  : degree ] ))
