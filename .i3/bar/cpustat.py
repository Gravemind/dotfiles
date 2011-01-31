#!/usr/bin/python3

import os

class CpuStat:
    ref = {
	'user'      : 'normal user processes',
	'nice'      : 'niced user processes',
	'sys'       : 'kernel mode processes',
	'idle'      : 'twiddling thumbs',
	'io'        : 'io wait',
	'irq'       : 'irq interrupts',
	'softirq'   : 'soft irq interrupts',
        };
    ticks = {}
    ticks_old = {}
    usage = {}

    filename = ''

    def __init__(self, cpu_sysfile = '/proc/stat'):
        self.filename = cpu_sysfile
        self.file = open(self.filename, 'r')

    def __del__(self):
        self.file.close()

    def update_stats(self, delay):
        while True:
            line = self.file.readline()
            if line[:3] != 'cpu':
                break
            position = 5
            if line[3].isnumeric():
                cpu = int(line[3])
            else:
                cpu = 'all'
            if cpu not in self.ticks_old: # first time
                self.ticks_old[cpu] = {}
            self.ticks[cpu] = {}
            self.ticks[cpu]['sum'] = 0;
            for key in self.ref:
                next_space = line[position:].index(' ')
                value = int(line[position : position + next_space])
                position += next_space + 1
                if key not in self.ticks_old[cpu]: # first time
                    self.ticks_old[cpu][key] = value
                    self.ticks[cpu][key] = 0
                else:
                    self.ticks[cpu][key] = value - self.ticks_old[cpu][key]
                    self.ticks_old[cpu][key] = value
                self.ticks[cpu]['sum'] += self.ticks[cpu][key]
            self.usage[cpu] = {}
            for key in self.ref:
                if self.ticks[cpu]['sum'] == 0:
                    self.usage[cpu][key] = 0
                else:
                    self.usage[cpu][key] = float(self.ticks[cpu][key]) * 100.0 / float(self.ticks[cpu]['sum'])
        self.file.seek(0)
