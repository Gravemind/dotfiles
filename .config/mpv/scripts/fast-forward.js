
var ff = 20;
var paused = true;

function fast_forward(data) {
    if (data.event == "down") {
        paused = mp.get_property("pause")
        mp.command("set play-direction forward; set speed " + ff + "; set pause no;")
    } else if (data.event == "up") {
        mp.command("set play-direction forward; set speed 1; set pause " + paused)
        // Fix playback continuing fast forward/backward for a bit
        mp.command("seek 0 relative")
    }
}

function fast_backward(data) {
    if (data.event == "down") {
        paused = mp.get_property("pause")
        mp.command("set play-direction backward; set speed " + ff + "; set pause no;")
    } else if (data.event == "up") {
        mp.command("set play-direction forward; set speed 1; set pause " + paused)
        // Fix playback continuing fast forward/backward for a bit
        mp.command("seek 0 relative")
    }
}

mp.add_key_binding(">", fast_forward, {complex: true});
mp.add_key_binding("<", fast_backward, {complex: true});
