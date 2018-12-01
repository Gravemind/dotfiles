
# https://gist.github.com/phiresky/8f4af83692b2915044cd3b01d28fc6e7
# https://www.svp-team.com/wiki/Manual:SVPflow

import vapoursynth as vs
from pprint import pprint

#core = vs.get_core(threads=9)
core = vs.get_core()
clip = video_in

core.std.LoadPlugin("/opt/svp/plugins/libsvpflow1_vs64.so")
core.std.LoadPlugin("/opt/svp/plugins/libsvpflow2_vs64.so")

enable = True

super_params="gpu:1, scale:{up:0}"

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

analyse_params=""

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

smooth_params="""
,algo:23,mask:{cover:80,area:0},scene:{limits:{m1:0,m2:0},mode:1}
"""
smooth_params=",algo:23,scene:{limits:{m1:0,m2:0},mode:1}"

smooth_params=""

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

print()
print("clip", clip.width, "x", clip.height, "at", container_fps, "fps")

max_fps = display_fps

# Limit FullHD at 120 fps
if clip.width >= 1920 and display_fps > 120:
    max_fps = 120.0

#max_fps = 60

# Interpolate to a multiple of the original source fps
dst_fps = container_fps
while dst_fps + container_fps <= max_fps:
    dst_fps += container_fps

#dst_fps = display_fps

if not enable or dst_fps <= container_fps:
    print("NOT reflowing clip", clip.width, "x", clip.height, "at", container_fps, "fps (display:", display_fps, ").")
else:
    src_fps_num = int(container_fps * 1e6)
    src_fps_den = int(1e6)
    dst_fps_num = int(dst_fps * 1e6)
    dst_fps_den = int(1e6)

    print("Reflowing clip", clip.width, "x", clip.height, " from ", src_fps_num/src_fps_den, "fps to", dst_fps_num/dst_fps_den, "fps (display:", display_fps, ").")

    clip = core.std.AssumeFPS(clip, fpsnum = src_fps_num, fpsden = src_fps_den)

    sup = core.svp1.Super(clip, "{"+super_params+"}")

    vectors = core.svp1.Analyse(sup["clip"], sup["data"], clip, "{"+analyse_params+"}")

    #pprint(vars(vectors))

    full_smooth_params = "{ rate: {num:"+str(dst_fps_num)+", den:"+str(dst_fps_den)+", abs:true} "+smooth_params+" }"
    #full_smooth_params = "{ rate: {num:2,den:1}, "+smooth_params+" }"

    clip = core.svp2.SmoothFps(clip, sup["clip"], sup["data"], vectors["clip"], vectors["data"], full_smooth_params)

    #pprint(vars(clip))

    clip = core.std.AssumeFPS(clip, fpsnum=clip.fps_num, fpsden=clip.fps_den)

clip.set_output()

print()
