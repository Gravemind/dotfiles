#!/usr/bin/python

# python3-mpd
# http://jatreuman.indefero.net/p/python-mpd/doc/
from mpd import (MPDClient, CommandError)

import pprint

class MPD:
    ok = False
    client = MPDClient()
    status = "stop"
    tags = []
    mpd_host = ""
    mpd_port = ""

    def __init__(self, host, port):
        self.mpd_host = host
        self.mpd_port = port
        self.connect()
        return

    def connect(self):
        try:
            self.client.connect(self.mpd_host, self.mpd_port)
        except Exception as err:
            print("MPD: connect fail:", err)
            self.ok = False
            return
        self.ok = True        

    def update(self):
        if self.ok == False:
            self.connect()
            if self.ok == False:
                self.status = 'stopped'
                self.tags = []
                return
        state = self.client.status()['state']
        if state != 'play':
            self.status = 'paused'
        else:
            self.status = 'playing'
        self.tags = self.client.currentsong()
        return

# def main():
#     mpd = MPD("127.0.0.1", "6600")
        
# if __name__ == "__main__":
#     main()
