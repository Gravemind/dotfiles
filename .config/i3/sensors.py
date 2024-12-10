#!/usr/bin/env python3

import sys
import os
import os.path
import subprocess
import json
import fnmatch
import collections

Sensor = collections.namedtuple("Sensor", "typ, chip, label")

# See `sensors.py --list`
SENSORS = {
    # "CPU": Sensor(typ="temp", chip="nct6795*", label="SMBUSMASTER*"),
    "CPU": Sensor(typ="temp", chip="k10temp*", label="Tctl"),
    "GPU": Sensor(typ="temp", chip="amdgpu*", label="edge*"),
}

HWMON_ROOT = "/sys/class/hwmon"

HWMON_TYPES = ["in", "fan", "temp", "freq"]

def read_line(path):
    try:
        with open(path, 'r') as f:
            out = f.read().strip()
    except FileNotFoundError:
        return None
    except OSError:
        return None
    return out

def main():
    here = os.path.dirname(__file__)

    sensors_dir = os.path.join(here, "sensors")
    try:
        os.mkdir(sensors_dir)
    except FileExistsError:
        pass

    for outname, cond in SENSORS.items():
        outpath = os.path.join(sensors_dir, outname)
        try:
            os.unlink(outpath)
        except FileNotFoundError:
            pass

    sensors = {}
    paths = {}

    for hwmonentry in os.scandir(HWMON_ROOT):
        if not hwmonentry.is_dir(follow_symlinks=True):
            continue

        chip = read_line(os.path.join(hwmonentry.path, "name"))
        if not chip:
            continue

        for sensorentry in os.scandir(hwmonentry):
            if not sensorentry.is_file(follow_symlinks=True):
                continue
            if sensorentry.name.endswith("_input"):
                name = sensorentry.name[:-len("_input")]
                value = read_line(sensorentry.path)
                if value is None:
                    continue
                label = read_line(os.path.join(hwmonentry.path, name + "_label"))
                if not label:
                    # print(f"label not found {sensorentry.path}", file=sys.stderr)
                    continue
                typ = None
                for t in HWMON_TYPES:
                    if name.startswith(t):
                        typ = t
                        break
                if not typ:
                    print(f"warning: unknown type {sensorentry.path}", file=sys.stderr)
                    continue

                key = Sensor(typ=typ, chip=chip, label=label)
                sensors[key] = value
                paths[key] = sensorentry.path

    for sensor, value in sensors.items():
        print(f"{sensor}\t{value}")

    found = {}

    for outname, cond in SENSORS.items():
        print(f"Looking for {outname} {cond}")

    for sensor in sensors.keys():
        for outname, cond in SENSORS.items():
            # print(sensor, cond)
            if not fnmatch.fnmatch(sensor.typ, cond.typ):
                continue
            if not fnmatch.fnmatch(sensor.chip, cond.chip):
                continue
            if not fnmatch.fnmatch(sensor.label, cond.label):
                continue

            target = paths[sensor]
            if outname in found:
                print(f"warning: Ignoring multiple match for sensor {outname!r} {cond}: {sensor} {target}", file=sys.stderr)
            else:
                outpath = os.path.join(sensors_dir, outname)
                print(f"Creating {outpath} -> {sensor} {target}")
                found[outname] = target
                os.symlink(target, outpath)

    # for outname, target in found.items():
    #     outpath = os.path.join(sensors_dir, outname)

main()
