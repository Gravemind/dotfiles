#!/usr/bin/python

import subprocess

def get_next_token(line, sep):
    index = line.find(sep)
    if index == -1:
        return ''
    return line[:index]

class CMus:
    status = ''
    tags = {}
    command = ''

    skip_words = ('tag', 'set')

    def __init__(self, command = 'cmus-remote -C status'):
        self.command = command

    def update(self):
        self.tags = {}
        out = subprocess.getoutput(self.command+' | cat -v')
        for line in out.splitlines():
            word = get_next_token(line, ' ')
            word = line[:len(word)]
            line = line[len(word) + 1:]
            while word in self.skip_words:
                word = get_next_token(line, ' ')
                word = line[:len(word)]
                line = line[len(word) + 1:]
            self.tags[word] = line
        if 'status' in self.tags:
            self.status = self.tags['status']
        else:
            self.status = 'stopped'
        return
