
# https://gist.github.com/phiresky/8f4af83692b2915044cd3b01d28fc6e7
# https://www.svp-team.com/wiki/Manual:SVPflow

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

    core.std.LoadPlugin("/opt/svp/plugins/libsvpflow1_vs64.so")
    core.std.LoadPlugin("/opt/svp/plugins/libsvpflow2_vs64.so")
    # core.std.LoadPlugin("/usr/lib/vapoursynth/libsvpflow2.so")
    # core.std.LoadPlugin("/home/jo/Downloads/svpflow-4.3.0.168/lib-linux/libsvpflow1_vs64.so")
    # core.std.LoadPlugin("/home/jo/Downloads/svpflow-4.3.0.168/lib-linux/libsvpflow2_vs64.so")

    enable = True

    super_params = "gpu:0, scale:{up:0}"

    analyse_params="""
    block: { w:16, overlap:2 },
    main: {
        search: {
            distance: -4,
            coarse: {
                satd: true,
                trymany: true
            }
        },
        penalty: {
            lambda: 60
        }
    },
    refine:[{thsad:200}]}
    """

    # analyse_params   = "block:{w:32},main:{search:{coarse:{type:2,distance:-6,bad:{sad:2000,range:24}},type:2}},refine:[{thsad:250}]"

    '''
    block: { w:16, overlap:2 },
    main: {
        search: {
            coarse: {
                trymany: true
            }
        },
        penalty: {
            lambda: 40
        }
    },
    refine:[{thsad:200}]}

    refine:[{thsad:250}]}
    block: { w:16, overlap:1 },
    main: {
        search: {
            type: 3,
            distance: -4,
            satd: false,
            sort: true,
            coarse: {
                satd: true,
                trymany: true,
                type: 4,
                distance: -8,
                bad: {
                    sad: 1500,
                    range: -12,
                    distance: -12
                }
            }
        },
        penalty: {
            lambda: 1
        }
    },


    block: { w:16, overlap:2 },
    block: { w:16, overlap:2 },
    block: { w:8, overlap: 2 },
    main: {
        search: {
            distance: -,
        }
    },
    refine: [{
        thsad: 1
    }]
    '''

    smoothfps_params="""
    ,algo:23,mask:{cover:80,area:0},scene:{limits:{m1:0,m2:0},mode:1}
    """
    smoothfps_params=",algo:23,scene:{limits:{m1:0,m2:0},mode:1}"

    '''
    algo:23,mask:{cover:60},scene:{limits:{m1:0,m2:0},mode:1}
    algo: 23
    cubic: true,
    mask: {
        area_sharp: 0.01
    },
    scene: {
        mode: 0
    }
    cubic: true,
    mask: {
        cover: 100,

    },
    scene: {
        mode: 1
    }
    scene: {
        mode: 1
    }
    '''

    # super_params     = "scale:{up:2},gpu:0,rc:false"
    # analyse_params   = "block:{w:32},main:{search:{coarse:{type:2,distance:-6,bad:{sad:2000,range:24}},type:2}},refine:[{thsad:250}]"
    # smoothfps_params = "rate:{num:5,den:2},algo:2,scene:{}"

    super_params     = "gpu:1"
    analyse_params   = "gpu:1"
    smoothfps_params = ",gpuid:11"

    if container_fps <= 0.1:
        log(f"abort container at {container_fps} fps")
        return

    log(f"Clip {clip.width}x{clip.height} {clip.format.name} at {container_fps} fps. Display at {display_fps} fps.")

    # display_fps = 144
    # display_fps = 143.995

    max_fps = display_fps

    if clip.width <= 1920:
        max_fps = min(max_fps, 144)
    else:
        max_fps = min(max_fps, 60)

    max_fps *= 1.001

    # Interpolate to a multiple of the original source fps
    i = 1
    while container_fps * float(i + 1) <= max_fps:
        i += 1
        # break
    dst_fps = container_fps * float(i)

    if not enable or dst_fps <= container_fps:
        log(f"NOT Reflowing clip")
    else:
        src_fps = container_fps
        src_fps_num = int(src_fps * 1e5)
        src_fps_den = int(1e5)
        dst_fps_num = int(dst_fps * 1e5)
        dst_fps_den = int(1e5)
        # src_fps_num = src_fps
        # src_fps_den = 1
        # dst_fps_num = dst_fps
        # dst_fps_den = 1

        log(f"Reflowing from {src_fps_num/src_fps_den} to {dst_fps_num/dst_fps_den} fps.")

        orig_clip = clip

        ENABLE_CONVERSION_YV12 = True
        ENABLE_CONVERSION_YV12 = False # Using vf=format=yuv420p gives better results !?
        ok = False
        while not ok:
            try:
                clip = core.std.AssumeFPS(clip, fpsnum = src_fps_num, fpsden = src_fps_den)
                sup = core.svp1.Super(clip, "{"+super_params+"}")
                ok = True
            except Exception as err:
                if ENABLE_CONVERSION_YV12 and str(err) == "SVSuper: Clip must be YV12":
                    log("Convert clip to YV12")
                    clip = orig_clip.resize.Bicubic(format=vs.YUV420P8) # convert to YV12
                else:
                    raise err
        if True:
            vectors = core.svp1.Analyse(sup["clip"], sup["data"], clip, "{"+analyse_params+"}")

            #plog(vars(vectors))

            full_smoothfps_params = "{ rate:{num:"+str(dst_fps_num)+", den:"+str(dst_fps_den)+", abs:true} "+smoothfps_params+" }"
            # full_smoothfp_params = "{ "+smoothfps_params+" }"

            clip = core.svp2.SmoothFps(clip, sup["clip"], sup["data"], vectors["clip"], vectors["data"], full_smoothfps_params)
            # clip = core.svp2.SmoothFps(clip, sup["clip"], sup["data"], vectors["clip"], vectors["data"],
            #                            full_smoothfps_params,
            #                            src=clip,fps=src_fps)
            # help(clip)
            # log("Got ", clip.fps_num / clip.fps_den)

            #plog(vars(clip))
        else:
            # help(core.svp2.SmoothFps_NVOF)
            # smooth  = core.svp2.SmoothFps_NVOF(input_m,smoothfps_params,nvof_src=input_m8,src=input_um,fps=src_fps)
            clip  = core.svp2.SmoothFps_NVOF(clip,smoothfps_params,nvof_src=clip,src=clip)#,fps=src_fps)
            # clip = core.svp2.SmoothFps_NVOF(clip, smoothfps_params, sup["clip"], sup["data"], "")

        clip = core.std.AssumeFPS(clip, fpsnum=clip.fps_num, fpsden=clip.fps_den)

    clip.set_output()

main()

print()
