#pragma once
#include "pc_core.h"

#define PLAYER_SHAPE_SIZE 32
typedef struct {
    int16_t x, y;
    uint16_t width, height;
    uint16_t rows[PLAYER_SHAPE_SIZE][2];
} PlayerShape;

/* Solid, inclusive triangle, split into two 16-pixel sprite columns.
 * Returns zero for geometry too large for the sprite buffers. */
unsigned player_shape_build(PlayerShape *shape, const int16_t x[3], const int16_t y[3]);
