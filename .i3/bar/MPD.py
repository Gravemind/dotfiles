#!/usr/bin/python

from mpd import (MPDClient, CommandError)
import pprint

class MPD:
    ok = False
    client = MPDClient()
    status = "stop"
    tags = []

    def __init__(self, host, port):
        self.connect()
        return

    def connect(self):
        try:
            self.client.connect(host, port)
        except:
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
