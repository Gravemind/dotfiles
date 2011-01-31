#!/usr/bin/python3

import datetime

class Timer:
    last_datetime = 0

    def __init__(self):
        self.reset()

    def elapsed_time(self):
        t = datetime.datetime.now() - self.last_datetime
        return float(t.seconds) + float(t.microseconds) / 1000000

    def reset(self):
        self.last_datetime = datetime.datetime.now()
