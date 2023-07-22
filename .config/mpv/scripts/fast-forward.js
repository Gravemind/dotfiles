
var ff = 20;

function fast_forward(data) {
    if (data.event == "down") {
        mp.command("set play-dir forward; set speed " + ff)
    } else if (data.event == "up") {
        mp.command("set play-dir forward; set speed 1")
    }
}

function fast_backward(data) {
    if (data.event == "down") {
        mp.command("set play-dir backward; set speed " + ff)
    } else if (data.event == "up") {
        mp.command("set play-dir forward; set speed 1")
    }
}

mp.add_key_binding(">", fast_forward, {complex: true});
mp.add_key_binding("<", fast_backward, {complex: true});
