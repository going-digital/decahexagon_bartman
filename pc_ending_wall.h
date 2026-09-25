#pragma once
#include "pc_ending_polygon.h"
#include "pc_projection.h"
/* Build an ordinary planar wall from a render-only merged radial span.
 * Directions are unscaled Q14 Cartesian unit vectors, already field-rotated.
 * Frame caller computes/reuses directions. Not final-scene 3D geometry. */
typedef struct { int16_t x,y; } PcEndingDirection;
int pc_ending_wall_quad(const PcSpan *span,int16_t pulse,
                        PcEndingDirection first,PcEndingDirection next,
                        PcEndingVertex output[4]);
