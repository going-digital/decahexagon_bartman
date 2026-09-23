#pragma once
#include "pc_core.h"

/* PC mode 0: 768x480, fx=665.107486138, fy=707.017178437, z=850.
 * Resize to 320x200: x=0.3260330814, y=0.3465770483 pixels/world unit.
 * Gameplay's Q8 zoom remains 128 (0.5); bake the remaining factors into
 * the Q14 direction tables once, instead of multiplying every vertex. */
#define VIEW_X_Q14 10683
#define VIEW_Y_Q14 11357
static inline int16_t view_scale_x(int16_t direction) {
    return (int16_t)(((int32_t)direction * VIEW_X_Q14) >> 14);
}
static inline int16_t view_scale_y(int16_t direction) {
    return (int16_t)(((int32_t)direction * VIEW_Y_Q14) >> 14);
}
