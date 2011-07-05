#!/usr/bin/python3

from netstat import NetStat
from timer import Timer
from cpustat import CpuStat
from memstat import MemStat
from sensors import Sensors
from amixer import AMixer
from cmus import CMus
from MPD import MPD
import dzen

import datetime
import dzen
import time
import os
import sys
import pprint

import traceback

time_rate = 1.0

ico_dir = os.path.join(os.getenv('HOME'), '.dzen/')
ico = {
    'stop'  : ico_dir + 'stop.xbm',
    'play'  : ico_dir + 'play.xbm',
    'pause' : ico_dir + 'pause.xbm',
    'music' : ico_dir + 'note.xbm',
    'up'    : ico_dir + 'net_up_02.xbm',
    'down'  : ico_dir + 'net_down_01.xbm',
    'cpu'   : ico_dir + 'cpu.xbm',
    'mem'   : ico_dir + 'mem.xbm',
    'temp'  : ico_dir + 'temp.xbm',
    'vol'   : ico_dir + 'spkr_01.xbm',
    }
col = {
    'fgoff' : '#777777',
    'fg1' : '#4C7899',
    'fg2' : '#3A5C75',
    'fg3' : '#2B4457',
    'fg4' : '#1B2A36',
    'fg5' : '#0A1014',
    'bg'  : '#000000',
    'bg1' : '#222222',
    'red'   : '#9E3A26',
    'green' : '#80AA83',
    }
separator = '^fg(' + col['bg1'] + ') ~ '
pico = 4
pbar = 5

d = dzen.DZen(dict(ico, **col))
timer = Timer()
net = NetStat('eth0')
cpu = CpuStat()
mem = MemStat()
sens = Sensors()
sens.update()
vol = AMixer('amixer get Master')
music = CMus()
# music = MPD("127.0.0.1", "6600")

i3wsbar = (
    '/usr/bin/i3-wsbar',
    '--show-all', '-c',
    'dzen2 -dock -expand left -x %x -fn -*-DejaVu\ Sans\ Mono-normal-normal-normal-*-13-*-*-*-*-*-*-*',
    '--input-on', 'DFP2',
    '-output-on', 'DFP2'
    )
pipe = os.pipe()
fd_r, fd_w = pipe
pid_child = os.fork()
if pid_child == 0:
    os.dup2(fd_r, 0)
    os.execve(i3wsbar[0], i3wsbar, os.environ)
    exit(0)

def disp_temp(d, *temps):
    temp = max(temps)
    if temp >= 60:
        d.fg('red')
    else:
        d.fg('green')
    d.a(str(temp))

def disp_error(title, d = None):
    if d != None:
        d.fg('red').a('ERROR ', title)
    print('{:-<50}{:->20}'
          .format('ERROR--' + title,
                  datetime.datetime.now().strftime('%m/%d-%H:%M:%S')))
    traceback.print_exception(*sys.exc_info())
    print('{:-^70}'.format(''))

try:

    delay = 0.0;
    sens_count = 6
    while True:
        delay = timer.elapsed_time()
        timer.reset()

        net.update_stats(delay)
        cpu.update_stats(delay)
        vol.update()
        music.update()
        if sens_count > 4:
            sens.update()
            mem.update_stats()
            sens_count = 0
        sens_count += 1

        d.reset()

        try:
            if music.status == 'stopped':
                d.fg('bg1').p(0, pico).i('stop').p(0, -pico)
            elif music.status != 'playing':
                d.fg('red').p(0, pico).i('pause').p(0, -pico)
            else:
                d.fg('green').p(0, pico).i('play').a(' ').i('music').p(0, -pico)
                d.fg('fg1')
                if 'artist' in music.tags and 'title' in music.tags:
                    d.a(' ', music.tags['artist']).fg('fg2').a(' ', music.tags['title'])
                elif 'file' in music.tags:
                    d.a(' ', music.tags['file'])
                else:
                    d.a(' ', '???')
        except:
            disp_error('music', d)

        d.pa('1113')
        d.a('   ')

        try:
            d.fg('fg1').a(str(net.rx_bytes).rjust(4))
            if net.rx_bit == 0:
                d.fg('fgoff')
            else:
                d.fg('green')
            d.p(2, pico).i('down').p(0, -pico)
            d.a(' ')
            d.fg('fg1').a(str(net.tx_bytes).rjust(4))
            if net.tx_bit == 0:
                d.fg('fgoff')
            else:
                d.fg('red')
            d.p(2, pico).i('up').p(0, -pico)
        except:
            disp_error('net', d)

        d.a(separator)

        try:
            d.fg('fg1').p(0, pico).i('cpu').p(0, -pico).p(5, pbar)
            cpubar = dzen.Bar(50, 6, 0, 100, 2, 1)
            cpubar.push(int(cpu.usage['all']['user'] +
                            cpu.usage['all']['nice'] +
                            cpu.usage['all']['sys']), col['fg1'])
            cpubar.push(int(cpu.usage['all']['io'] +
                            cpu.usage['all']['irq'] +
                            cpu.usage['all']['softirq']), col['fg3'])
            d.a(cpubar.draw(col['bg1'])).p(0, -pbar)
            d.a(' ')
        except:
            disp_error('cpu', d)

        try:
            d.fg('fg1').p(0, pico).i('mem').p(0, -pico).p(5, pbar)
            membar = dzen.Bar(50, 6, 0, mem.usages['total'], 2, 1)
            membar.push(mem.usages['total']
                        - mem.usages['free']
                        - mem.usages['buff']
                        - mem.usages['cached'], col['fg1'])
            membar.push(mem.usages['buff'], col['fg3'])
            membar.push(mem.usages['cached'], col['fg4'])
            d.a(membar.draw(col['bg1'])).p(0, -pbar)
        except:
            disp_error('mem', d)

        d.a(separator)

        try:
            d.fg('fg1').p(0, pico).i('temp').p(0, -pico).a(' ')
            disp_temp(d, sens.temp['atk0110-acpi-0']['MB Temperature'])
            # d.p(5, 0)
            # disp_temp(d, sens.temp['coretemp-isa-0000']['Core 0'],
            #           sens.temp['coretemp-isa-0001']['Core 1'])
            # d.p(5, 0)
            # disp_temp(d, sens.temp['radeon-pci-0300']['temp1'],
            #           sens.temp['radeon-pci-0400']['temp1'])
        except:
            disp_error('sensors', d)

        d.a(separator)

        try:
            d.fg('fg1').p(0, pico).i('vol').p(5, -pico).p(0, pbar)
            volbar = dzen.Bar(50, 6, 0, 100, 2, 1)
            volbar.push(vol.vol['Master'], col['fg1'])
            d.a(volbar.draw(col['bg1'])).p(0, -pbar)
        except:
            disp_error('amixer', d)

        d.a(separator)

        d.fg('fg2').a(datetime.datetime.now().strftime('%d/%m %H:%M'))

        os.write(fd_w, str(d.out + '\n').encode('ASCII'))

        delay2 = timer.elapsed_time()
        if delay2 < time_rate:
            time.sleep(time_rate - delay2)

except:
    disp_error('ERROR')
finally:
    os.kill(pid_child, 9)
