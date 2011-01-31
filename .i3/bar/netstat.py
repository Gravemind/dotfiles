#!/usr/bin/python3

import os

class NetStat:
    rx_bit = 0
    tx_bit = 0
    rx_bytes = 0
    tx_bytes = 0
    rx_old = 0
    tx_old = 0

    def __init__(self, interface = 'eth0', rx_file = None, tx_file = None):
        self.interface = interface
        if rx_file == None:
            rx_file = os.path.join('/sys/class/net', interface, 'statistics/rx_bytes')
        if tx_file == None:
            tx_file = os.path.join('/sys/class/net', interface, 'statistics/tx_bytes')
        self.file_rx = open(rx_file)
        self.file_tx = open(tx_file)
        self.update_stats(0.0)

    def __del__(self):
        self.file_rx.close()
        self.file_tx.close()

    def update_stats(self, delay):
        rx = int(self.file_rx.read())
        tx = int(self.file_tx.read())
        if delay > 0.0:
            self.rx_bit = int((rx - self.rx_old) / delay)
            self.tx_bit = int((tx - self.tx_old) / delay)
            self.rx_bytes = self.rx_bit // 1024
            self.tx_bytes = self.tx_bit // 1024
        self.rx_old = rx
        self.tx_old = tx
        self.file_rx.seek(0)
        self.file_tx.seek(0)

