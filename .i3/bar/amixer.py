#!/usr/bin/python3

import subprocess

class AMixer:
    vol = {}
    command = {}

    def __init__(self, command = 'amixer'):
        self.command = command

    def update(self):
        vol = {}
        out = subprocess.getoutput(self.command)
        secteur = '?'
        for line in out.splitlines():
            if line[0] == ' ':
                pos2 = line.find('%]')
                if pos2 != -1:
                    self.vol[secteur] = int(line[
                            line[pos2 - 4:pos2].rfind('[') + pos2 - 3
                            :pos2])
            else:
                if line.count("'") == 2:
                    secteur = line[line.find("'") + 1:line.rfind("'")]
                    self.vol[secteur] = {}
