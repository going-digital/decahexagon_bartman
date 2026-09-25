#pragma once
#include "pc_ending_polygon.h"
#include "pc_ending_affine.h"
/* Submit one prepared ending quad as one-dot XOR fill seeds. Caller owns
 * back-buffer clearing, blit_line_mode(), final blit_fill() and display swap.
 * Returns the prepared vertex count, 0 invisible, -1 rejected; rejected quads
 * issue no blitter commands. This is not a complete ending frame renderer. */
int render_ending_quad(void *buffer,const PcEndingView *view,
                       const PcEndingVertex vertices[4]);
/* Fast planar alternative. Matrix is prepared/cached by the frame caller.
 * Uses existing edge clipping and fill parity, including viewport-enclosing
 * polygons. Returns 1 submitted, 0 rejected with no drawing. */
int render_ending_affine_quad(void *buffer,const PcEndingAffine *matrix,
                              const PcEndingVertex vertices[4]);

#include "pc_ending_wall.h"
/* Span must come from render-only radial union, not raw overlapping records. */
int render_ending_affine_wall(void *buffer,const PcEndingAffine *matrix,
                              const PcSpan *span,int16_t pulse,
                              PcEndingDirection first,PcEndingDirection next);

#include "pc_ending_scene.h"
int render_ending_affine_walls(void *buffer,PcEndingScene *scene,const PcWorld *world,
                               const PcEndingView *view,unsigned sides,int16_t pulse,
                               const PcEndingDirection *directions,unsigned *rejected);
int render_ending_affine_hub(void *buffer,const PcEndingAffine *matrix,unsigned sides,
                             int16_t radius,const PcEndingDirection *directions);
/* CPU solid overlay after the wall/hub fill has completed. Clips to viewport.
 * Frame owner must hide ordinary player sprites when using this path. */
int render_ending_affine_player(void *buffer,const PcEndingAffine *matrix,
                                const PcEndingDirection directions[3],
                                int16_t tip_radius,int16_t base_radius);

/* Complete planar draw into an undisplayed buffer. Owns clear/seed/fill/player
 * ordering, but not copper/sprite changes or buffer swapping. Caller must hide
 * ordinary player sprites. A successful return does not imply zero rejected
 * geometry; inspect the report. World/direction data must remain stable. */
typedef struct { unsigned walls,rejected_walls; unsigned hub,player; } EndingFrameReport;
int render_ending_flat_frame(void *buffer,PcEndingScene *scene,const PcWorld *world,
                             const PcEndingView *view,unsigned sides,int16_t pulse,
                             const PcEndingDirection *directions,int16_t hub_radius,
                             const PcEndingDirection player_directions[3],
                             int16_t tip_radius,int16_t base_radius,EndingFrameReport *report);
