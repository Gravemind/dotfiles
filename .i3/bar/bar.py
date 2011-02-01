#!/usr/bin/python3

from netstat import NetStat
from timer import Timer
from cpustat import CpuStat
from memstat import MemStat
from sensors import Sensors
from amixer import AMixer
from cmus import CMus
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
    'fg'  : '#7799AA',
    'fg1' : '#557799',
    'fg2' : '#335566',
    'fg3' : '#888888',
    'fg4' : '#223945',
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
cmus = CMus()

i3wsbar = (
    '/usr/bin/i3-wsbar',
    '--show-all', '-c',
    'dzen2 -dock -expand left -x %x -fn -*-DejaVu\ Sans\ Mono-normal-normal-normal-*-13-*-*-*-*-*-*-*',
    '--input-on', 'DVI-0',
    '-output-on', 'DVI-0'
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


try:

    delay = 0.0;
    sens_count = 6
    while True:
        delay = timer.elapsed_time()
        timer.reset()

        net.update_stats(delay)
        cpu.update_stats(delay)
        vol.update()
        cmus.update()
        if sens_count > 4:
            sens.update()
            mem.update_stats()
            sens_count = 0
        sens_count += 1

        d.reset()

        if len(cmus.status) == 0:
            d.fg('bg1').p(0, pico).i('stop').p(0, -pico)
        elif cmus.status['status'] != 'playing':
            d.fg('red').p(0, pico).i('pause').p(0, -pico)
        else:
            d.fg('green').p(0, pico).i('play').a(' ').i('music').p(0, -pico)
            d.fg('fg1').a(' ', cmus.status['artist'])
            d.fg('fg2').a(' ', cmus.status['title'])

        d.pa('1113')
        d.a('   ')

        d.fg('fg1').a(str(net.rx_bytes).rjust(4))
        if net.rx_bit == 0:
            d.fg('fg3')
        else:
            d.fg('green')
        d.p(2, pico).i('down').p(0, -pico)
        d.a(' ')
        d.fg('fg1').a(str(net.tx_bytes).rjust(4))
        if net.tx_bit == 0:
            d.fg('fg3')
        else:
            d.fg('red')
        d.p(2, pico).i('up').p(0, -pico)

        d.a(separator)

        d.fg('fg2').p(0, pico).i('cpu').p(0, -pico).p(5, pbar)
        cpubar = dzen.Bar(50, 6, 0, 100, 2, 1)
        cpubar.push(int(cpu.usage['all']['user'] +
                        cpu.usage['all']['nice'] +
                        cpu.usage['all']['sys']), col['fg2'])
        cpubar.push(int(cpu.usage['all']['io'] +
                        cpu.usage['all']['irq'] +
                        cpu.usage['all']['softirq']), col['fg4'])
        d.a(cpubar.draw(col['bg1'])).p(0, -pbar)
        d.a(' ')

        d.fg('fg2').p(0, pico).i('mem').p(0, -pico).p(5, pbar)
        membar = dzen.Bar(50, 6, 0, mem.usages['total'], 2, 1)
        membar.push(mem.usages['total']
                    - mem.usages['free']
                    - mem.usages['cached'], col['fg2'])
        membar.push(mem.usages['cached'], col['fg4'])
        d.a(membar.draw(col['bg1'])).p(0, -pbar)

        d.a(separator)

        d.fg('fg2').p(0, pico).i('temp').p(0, -pico).a(' ')
        disp_temp(d, sens.temp['atk0110-acpi-0']['MB Temperature'])
        d.p(5, 0)
        disp_temp(d, sens.temp['coretemp-isa-0000']['Core 0'],
                  sens.temp['coretemp-isa-0001']['Core 1'])
        d.p(5, 0)
        disp_temp(d, sens.temp['radeon-pci-0300']['temp1'],
                  sens.temp['radeon-pci-0400']['temp1'])

        d.a(separator)

        d.fg('fg2').p(0, pico).i('vol').p(5, -pico).p(0, pbar)
        volbar = dzen.Bar(50, 6, 0, 100, 2, 1)
        volbar.push(vol.vol['Master'], col['fg2'])
        d.a(volbar.draw(col['bg1'])).p(0, -pbar)

        d.a(separator)

        d.fg('fg').a(datetime.datetime.now().strftime('%d/%m %H:%M'))

        os.write(fd_w, str(d.out + '\n').encode('ASCII'))

        delay2 = timer.elapsed_time()
        if delay2 < time_rate:
            time.sleep(time_rate - delay2)

except:
    print('{:-^70}'.format('ERROR'))
    traceback.print_exception(*sys.exc_info())
    print('{:-^70}'.format(''))
finally:
    os.kill(pid_child, 9)
