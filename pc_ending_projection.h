#pragma once
#include "pc_core.h"
typedef struct {
    int16_t tilt,otis,depth,centre_x,centre_y;
    uint16_t focal_x,focal_y;
} PcEndingView;
/* Stage-4 projection of integral PC world coordinates. Returns zero for
 * invalid input, leaving outputs unchanged. Valid coordinates/depth: +/-8192;
 * tilt: +/-90; focal scales: 0..4096. Output saturates to +/-32767.
 * Saturation is only an arithmetic guard: callers still need polygon clipping.
 * Uses 64-bit integer arithmetic; performance has not been measured on Amiga. */
int pc_ending_project(const PcEndingView *view,int16_t x,int16_t y,int16_t z,
                      int16_t *screen_x,int16_t *screen_y);

/* Unsaturated coordinates for clipping. Fails without changing outputs when
 * either result exceeds +/-1,000,000,000; callers must reject that polygon. */
int pc_ending_project_wide(const PcEndingView *view,int16_t x,int16_t y,int16_t z,
                           int32_t *screen_x,int32_t *screen_y);
