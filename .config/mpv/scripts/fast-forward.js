
var ff_speed = 20;
var prev_speed = 1;
var prev_paused = true;

function fast_forward(data) {
    if (data.event == "down") {
        prev_speed = mp.get_property("speed")
        prev_paused = mp.get_property("pause")
        mp.command("set play-direction forward; set speed " + ff_speed + "; set pause no;")
    } else if (data.event == "up") {
        mp.command("set play-direction forward; set speed " + prev_speed + "; set pause " + prev_paused)
        // Fix playback continuing fast forward/backward for a bit
        mp.command("seek 0 relative")
    }
}

function fast_backward(data) {
    if (data.event == "down") {
        prev_speed = mp.get_property("speed")
        prev_paused = mp.get_property("pause")
        mp.command("set play-direction backward; set speed " + ff_speed + "; set pause no;")
    } else if (data.event == "up") {
        mp.command("set play-direction forward; set speed " + prev_speed + "; set pause " + prev_paused)
        // Fix playback continuing fast forward/backward for a bit
        mp.command("seek 0 relative")
    }
}

mp.add_key_binding(">", fast_forward, {complex: true});
mp.add_key_binding("<", fast_backward, {complex: true});
