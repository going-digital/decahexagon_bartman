#pragma once
#include "pc_core.h"
typedef struct {
    int16_t x0,y0,x1,y1,fix_y0,fix_y1;
    uint8_t line,fix;
} RenderClip;
/* Clip fill-seed edges to 320x200; preserve right-edge XOR parity.
 * Inputs from projection are within +/-8191 (differences fit signed16). */
void render_clip_line(int16_t x0,int16_t y0,int16_t x1,int16_t y1,RenderClip *out);
