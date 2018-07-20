# https://github.com/haasn/gentoo-conf/blob/xor/home/nand/.mpv/filters/mvtools.vpy
# https://gist.github.com/phiresky/4bfcfbbd05b3c2ed8645
#
# http://avisynth.org.ru/mvtools/mvtools.html

import vapoursynth as vs

core = vs.get_core()
clip = video_in

enable = True

blockSize = 32

analyseParams = {
    'blksize': blockSize,
    'search': 3,
    'searchparam': 2,
    #'trymany': 1,
    #'meander': 100,
    #'overlap': 8,
    #'pelsearch': 8,
    #'dct': 5,
}
recalculateParams = [

    {
        **analyseParams,
        'blksize': 16,
        # 'thsad': 2,
        # 'searchparam': 3,
    },

    # {
    #     **analyseParams,
    #     'blksize': 8,
    #     'thsad': 0,
    #     'searchparam': 4,
    # },

    # {
    #     **analyseParams,
    #     'blksize': 32,
    #     'thsad': 0,
    #     'searchparam': 2,
    # },

    # {
    #     **analyseParams,
    #     'blksize': blockSize/4,
    #     'thsad': 20,
    #     'searchparam': 2,
    # },

]

fpsParams = {
    # note: mode 3 seems better than 4 or 0
    #'mode': 0, # default
    #'mode': 2,

    #'ml': 100,

    # threshold for full screen scene change detection
    # note: 130 (default) makes some scene transition artefacty
    # note: 12 makes some motion jerky
    # 'thscd2': 130,
    # 'thscd1': 200,

    #'thscd1': ,
    #'thscd2': 140,
}

dst_fps = display_fps
# # Interpolating to fps higher than 60 is too CPU-expensive, smoothmotion can handle the rest.
# while (dst_fps > 60):
#     dst_fps /= 2

print()
print("clip", clip.width, "x", clip.height, "at", container_fps, "fps")

if not enable or clip.width > 1920 or clip.height > 1200 or container_fps > 49:
    print("NOT reflowing clip", clip.width, "x", clip.height, "at", container_fps, "fps")
else:
    src_fps_num = int(container_fps * 1e4)
    src_fps_den = int(1e4)
    dst_fps_num = int(dst_fps * 1e4)
    dst_fps_den = int(1e4)

    print("Reflowing from", src_fps_num/src_fps_den, "fps to", dst_fps_num/dst_fps_den, "fps.")

    clip = core.std.AssumeFPS(clip, fpsnum = src_fps_num, fpsden = src_fps_den)

    sup  = core.mv.Super(clip, pel=2, hpad=blockSize, vpad=blockSize)

    bvec = core.mv.Analyse(sup, isb=True, **analyseParams)
    fvec = core.mv.Analyse(sup, isb=False, **analyseParams)

    for recal in recalculateParams:
        bvec = core.mv.Recalculate(sup, bvec, **recal)
        fvec = core.mv.Recalculate(sup, fvec, **recal)

    clip = core.mv.FlowFPS(clip, sup, bvec, fvec, den=0, **fpsParams)
    #clip = core.mv.BlockFPS(clip, sup, bvec, fvec, den=0, mode=5)

clip.set_output()

print()
