#!/usr/bin/python3

import os

class MemStat:
    ref = {
        'total'   : 'MemTotal',
        'free'    : 'MemFree',
        'cached'  : 'Cached',
        }
    usages = {}
    usage = 0

    def __init__(self, filename='/proc/meminfo'):
        self.file = open(filename, 'r')

    def __del__(self):
        self.file.close()

    def update_stats(self):
        self.usages = {}
        while True:
            line = self.file.readline()
            i = line.find(':')
            if i == -1:
                break
            word = line[:i]
            for key in self.ref:
                if word == self.ref[key]:
                    size = int(line[len(word) + 1:-3].lstrip())
                    self.usages[key] = size
                    break
            if len(self.usages) == len(self.ref):
                break
        self.usage = ((self.usages['total'] - self.usages['free']
                       - self.usages['cached']) * 100) / self.usages['total']
        self.file.seek(0)
