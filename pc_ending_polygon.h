#pragma once
#include "pc_ending_projection.h"
#include "pc_ending_clip.h"
typedef struct { int16_t x,y,z; } PcEndingVertex;
/* Prepare one quad without publishing partial results. Positive return is
 * clipped vertex count, zero invisible/degenerate, -1 invalid or nonconvex.
 * Output is unchanged unless a visible polygon is returned. Depth reflection
 * may fold a quad; such polygons need a separate path, not convex clipping. */
int pc_ending_polygon(const PcEndingView *view,const PcEndingVertex vertices[4],
                      int16_t xmax,int16_t ymax,
                      PcEndingPoint output[PC_END_CLIP_CAPACITY]);
