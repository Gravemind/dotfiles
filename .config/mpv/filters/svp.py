
# https://gist.github.com/phiresky/8f4af83692b2915044cd3b01d28fc6e7
# https://www.svp-team.com/wiki/Manual:SVPflow
# https://github.com/hooke007/mpv_PlayKit/blob/main/k7sfunc.py#L1624

import vapoursynth as vs
from pprint import pprint

print()

def log(*args):
    print("svp.py:", *args)

def main() :
    #core = vs.get_core(threads=9)
    # core = vs.get_core()
    core = vs.core
    core.num_threads = 6
    clip = video_in

    # core.std.LoadPlugin("/opt/svp/plugins/libsvpflow1.so")
    # core.std.LoadPlugin("/opt/svp/plugins/libsvpflow2.so")
    core.std.LoadPlugin("/usr/lib/vapoursynth/libsvpflow1.so")
    core.std.LoadPlugin("/usr/lib/vapoursynth/libsvpflow2.so")

    enable = True

    super_params     = "gpu:1"
    analyse_params   = ""
    smoothfps_params = ",gpuid:0"

    if container_fps <= 1:
        log(f"Wont reflow: abort container at {container_fps} fps")
        return

    log(f"Clip {clip.width}x{clip.height} {clip.format.name} at {container_fps} fps. Display at {display_fps} fps.")
    if not enable:
        log("Wont reflow: disabled")
        return

    max_fps = display_fps
    # max_fps = 60
    if clip.width <= 1920:
        max_fps = min(max_fps, 144)
    else:
        max_fps = min(max_fps, 60)
    max_fps *= 1.001

    # Interpolate to a multiple of the original source fps
    rate = 1
    while container_fps * float(rate + 1) <= max_fps:
        rate += 1
        # break
    if rate == 1:
        log("Wont reflow: rate = 1")
        return

    src_fps = container_fps
    dst_fps = container_fps * float(rate)
    src_fps_num = int(src_fps * 1e6)
    src_fps_den = int(1e6)
    dst_fps_num = int(dst_fps * 1e6)
    dst_fps_den = int(1e6)
    log(f"Reflowing x{rate} from {src_fps} to {dst_fps} fps.")

    orig_clip = clip

    clip = core.std.AssumeFPS(clip, fpsnum = src_fps_num, fpsden = src_fps_den)
    # clip = orig_clip

    ENABLE_CONVERSION_YV12 = True
    # ENABLE_CONVERSION_YV12 = False # Using vf=format=yuv420p gives better results !?
    try:
        sup = core.svp1.Super(clip, "{"+super_params+"}")
    except Exception as err:
        if ENABLE_CONVERSION_YV12 and str(err) == "SVSuper: Clip must be YV12":
            log("Convert clip to YV12")
            clip8 = clip.resize.Bicubic(format=vs.YUV420P8) # convert to YV12
            sup = core.svp1.Super(clip8, "{"+super_params+"}")
        else:
            raise

    if True:
        vectors = core.svp1.Analyse(sup["clip"], sup["data"], clip, "{"+analyse_params+"}")

        full_smoothfps_params = "{ rate:{num:"+str(rate)+", den:1, abs:false} "+smoothfps_params+" }"

        clip = core.svp2.SmoothFps(clip, sup["clip"], sup["data"], vectors["clip"], vectors["data"], full_smoothfps_params)

    else:
        clip  = core.svp2.SmoothFps_NVOF(clip,smoothfps_params,nvof_src=clip,src=clip)#,fps=src_fps)

    clip.set_output()

main()

print()
