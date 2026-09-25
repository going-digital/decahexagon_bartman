#pragma once
#include "pc_ending_affine.h"
#include "pc_ending_wall.h"
/* Frame-owned scratch; allocate once, not on the small Amiga stack. */
typedef struct { PcEndingAffineCache camera; PcSpan spans[PC_WALL_CAPACITY]; } PcEndingScene;
typedef void (*PcEndingEdges)(void *context,const int16_t xs[4],const int16_t ys[4]);
/* Planar wall pass only. Returns submitted quads, -1 for invalid frame inputs.
 * Rejected spans are counted explicitly. Does not mutate simulation records.
 * Callback consumes arrays synchronously and must not retain their addresses. */
int pc_ending_scene_walls(PcEndingScene *scene,const PcWorld *world,
                         const PcEndingView *view,unsigned sides,int16_t pulse,
                         const PcEndingDirection *directions,PcEndingEdges emit,
                         void *context,unsigned *rejected);
/* Prepare one closed hub perimeter, not overlapping triangle fans. Radius
 * includes the caller's pulse. Returns sides on success, zero on rejection.
 * Output arrays have six entries and are unchanged on failure. */
unsigned pc_ending_scene_hub(const PcEndingAffine *matrix,unsigned sides,int16_t radius,
                             const PcEndingDirection *directions,int16_t xs[6],int16_t ys[6]);

#include "player_shape.h"
/* Directions: nose, left base, right base (unscaled Q14, already rotated).
 * Uses existing Amiga marker dimensions supplied by caller, not PC 3D geometry.
 * Returns zero and clears width/height if invalid or larger than 32x32. */
unsigned pc_ending_scene_player(const PcEndingAffine *matrix,
                                const PcEndingDirection directions[3],
                                int16_t tip_radius,int16_t base_radius,PlayerShape *shape);
