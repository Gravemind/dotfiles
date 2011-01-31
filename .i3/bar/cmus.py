#!/usr/bin/python

import subprocess

def get_next_token(line, sep):
    index = line.find(sep)
    if index == -1:
        return ''
    return line[:index]

class CMus:
    status = {}
    command = ''

    skip_words = ('tag', 'set')

    def __init__(self, command = 'cmus-remote -C status'):
        self.command = command

    def update(self):
        self.status = {}
        out = subprocess.getoutput(self.command)
        for line in out.splitlines():
            word = get_next_token(line, ' ')
            word = line[:len(word)]
            line = line[len(word) + 1:]
            while word in self.skip_words:
                word = get_next_token(line, ' ')
                word = line[:len(word)]
                line = line[len(word) + 1:]
            self.status[word] = line
