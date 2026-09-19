#pragma once
#include "pc_core.h"
/* Selected once from GfxBase->DisplayFlags, before HUD/copper/audio setup. */
typedef struct {
    uint16_t rate, lines, visible_height, music_period, sfx_period;
} VideoTiming;
extern VideoTiming video_timing;
void video_select(unsigned pal);
