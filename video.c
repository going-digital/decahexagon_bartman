#include "video.h"
VideoTiming video_timing;
void video_select(unsigned pal) {
    video_timing.rate=pal?50:60;
    video_timing.lines=pal?312:262;
    video_timing.visible_height=pal?256:200;
    video_timing.music_period=pal?296:298;
    video_timing.sfx_period=pal?443:447;
}
