#pragma once
#include "pc_core.h"
typedef struct { int32_t x,y; } PcEndingPoint;
#define PC_END_CLIP_CAPACITY 8
/* Clip a convex projected quad to inclusive viewport bounds. Returns vertex
 * count (0 means invisible or degenerate). Input/output may alias. Output has
 * room for PC_END_CLIP_CAPACITY points. Coordinates must be within +/-1,000,000,000. */
unsigned pc_ending_clip_quad(const PcEndingPoint quad[4],int16_t xmax,int16_t ymax,
                            PcEndingPoint output[PC_END_CLIP_CAPACITY]);
