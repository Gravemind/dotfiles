#!/bin/bash

set -euo pipefail

ALWAYS_REFRESH_EVERY_SEC=$(( 60 * 5 ))
#ALWAYS_REFRESH_EVERY_SEC=1

NOTIFY_EVERY_BAT_PERCENT=10
LOW_BAT_THRESHOLD_PERCENT=20

_LAST_MANGLE=""
notify_update() {
    local input_pties_file="$1"

    local adp=0
    local bat=0
    local status="Unknown"
    local cap=-1
    while read -u3 -r line
    do
        k="${line%%=*}"
        v="${line#*=}"
        case "$k" in
            POWER_SUPPLY_ONLINE) ## assume only appears for AC power adapters
                adp="$v"
                ;;
            POWER_SUPPLY_PRESENT) ## assume only appears for batteries
                bat="$v"
                ;;
            POWER_SUPPLY_STATUS) ## assume only appears for batteries
                status="$v"
                ;;
            POWER_SUPPLY_CAPACITY)
                cap="$v"
                ;;
        esac
    done 3< "$input_pties_file"

    local msg=""
    local icon=""
    local urg="normal"
    if [[ "$bat" -eq 1 ]]; then
        icon="battery-"
        if [[ "$cap" -eq 100 ]]; then
            icon+="full"
        elif [[ "$cap" -gt $LOW_BAT_THRESHOLD_PERCENT ]]; then
            icon+="good"
        else
            icon+="low"
            urg="critical"
        fi
        if [[ "$adp" -eq 1 ]]; then
            msg="Charging"
            icon+="-charging"
            urg="low"
        else
            msg="Discharging"
        fi
    else
        msg="No battery"
        icon="battery-missing"
    fi

    icon+="-symbolic.symbolic"

    local force=0

    local refresh_every_pcent=10
    local mangle="$icon $status $urg $(( cap / NOTIFY_EVERY_BAT_PERCENT ))"
    if [[ $force -eq 1 || "$_LAST_MANGLE" != "$mangle" ]]; then
        _LAST_MANGLE="$mangle"

        notify "BAT" -i "$icon" -u "$urg" "$msg" "$status, ${cap}%"
    fi
}

notify() {
    local id="$1"
    shift
    notify-send -a daemon_battery -h "string:synchronous:$id" "$@"
}

udev_monitor() {
    while true; do
        timeout $ALWAYS_REFRESH_EVERY_SEC udevadm monitor -s power_supply -u || true
        echo "dummy line for refresh"
    done
}

udev_pties() {
    udevadm info -q property /sys/class/power_supply/*
}

run_tests() {
    declare -A TESTS

    TESTS[discharging_good]="
DEVPATH=/devices/LNXSYSTM:00/LNXSYBUS:00/ACPI0003:00/power_supply/ADP0
POWER_SUPPLY_NAME=ADP0
POWER_SUPPLY_ONLINE=0
SUBSYSTEM=power_supply
DEVPATH=/devices/LNXSYSTM:00/LNXSYBUS:00/PNP0C0A:00/power_supply/BAT0
POWER_SUPPLY_NAME=BAT0
POWER_SUPPLY_STATUS=Discharging
POWER_SUPPLY_PRESENT=1
POWER_SUPPLY_TECHNOLOGY=Li-ion
POWER_SUPPLY_CYCLE_COUNT=0
POWER_SUPPLY_VOLTAGE_MIN_DESIGN=11100000
POWER_SUPPLY_VOLTAGE_NOW=11712000
POWER_SUPPLY_CURRENT_NOW=1621000
POWER_SUPPLY_CHARGE_FULL_DESIGN=5200000
POWER_SUPPLY_CHARGE_FULL=694000
POWER_SUPPLY_CHARGE_NOW=642000
POWER_SUPPLY_CAPACITY=92
POWER_SUPPLY_CAPACITY_LEVEL=Normal
POWER_SUPPLY_MODEL_NAME=Dell
POWER_SUPPLY_MANUFACTURER=SANYO
POWER_SUPPLY_SERIAL_NUMBER=1233
SUBSYSTEM=power_supply
"

    TESTS[nobattery]="
DEVPATH=/devices/LNXSYSTM:00/LNXSYBUS:00/ACPI0003:00/power_supply/ADP0
POWER_SUPPLY_NAME=ADP0
POWER_SUPPLY_ONLINE=1
SUBSYSTEM=power_supply
"

    TESTS[charging]="
DEVPATH=/devices/LNXSYSTM:00/LNXSYBUS:00/ACPI0003:00/power_supply/ADP0
POWER_SUPPLY_NAME=ADP0
POWER_SUPPLY_ONLINE=1
SUBSYSTEM=power_supply
DEVPATH=/devices/LNXSYSTM:00/LNXSYBUS:00/PNP0C0A:00/power_supply/BAT0
POWER_SUPPLY_NAME=BAT0
POWER_SUPPLY_STATUS=Full
POWER_SUPPLY_PRESENT=1
POWER_SUPPLY_TECHNOLOGY=Li-ion
POWER_SUPPLY_CYCLE_COUNT=0
POWER_SUPPLY_VOLTAGE_MIN_DESIGN=11100000
POWER_SUPPLY_VOLTAGE_NOW=12109000
POWER_SUPPLY_CURRENT_NOW=1000
POWER_SUPPLY_CHARGE_FULL_DESIGN=5200000
POWER_SUPPLY_CHARGE_FULL=694000
POWER_SUPPLY_CHARGE_NOW=694000
POWER_SUPPLY_CAPACITY=100
POWER_SUPPLY_CAPACITY_LEVEL=Full
POWER_SUPPLY_MODEL_NAME=Dell
POWER_SUPPLY_MANUFACTURER=SANYO
POWER_SUPPLY_SERIAL_NUMBER=1233
SUBSYSTEM=power_supply
"

    # fake test
    TESTS[discharging_low]="
DEVPATH=/devices/LNXSYSTM:00/LNXSYBUS:00/ACPI0003:00/power_supply/ADP0
POWER_SUPPLY_NAME=ADP0
POWER_SUPPLY_ONLINE=0
SUBSYSTEM=power_supply
DEVPATH=/devices/LNXSYSTM:00/LNXSYBUS:00/PNP0C0A:00/power_supply/BAT0
POWER_SUPPLY_NAME=BAT0
POWER_SUPPLY_STATUS=Discharging
POWER_SUPPLY_PRESENT=1
POWER_SUPPLY_CAPACITY=10
"

    for n in "${!TESTS[@]}"
    do
        echo "TEST $n"
        notify_update <( echo "${TESTS[$n]}" )
        sleep 2
    done
}

main() {
    if [[ "${1:-}" = "test" ]]; then
        run_tests
        return
    fi
    while read -u3 -r line
    do
        notify_update <( udev_pties )
    done 3< <( udev_monitor )
}

main "$@"
