#pragma once
#include "pc_ending_projection.h"
#include "pc_ending_polygon.h"
/* Planar z=0 approximation: replace varying perspective depth by centre depth.
 * Q12 matrix; prepare once per frame. Not for nonplanar/final-scene geometry. */
typedef struct { int16_t a,b,d,x,y; } PcEndingAffine;
int pc_ending_affine_prepare(PcEndingAffine *out,const PcEndingView *view);
/* Coordinates +/-8192. No divisions except power-of-two scaling per point. */
int pc_ending_affine_point(const PcEndingAffine *matrix,int16_t x,int16_t y,
                           int32_t *screen_x,int32_t *screen_y);
/* Presentation cache enforces tilt=0 and Otis=0. Direct prepare remains
 * available for comparison tools. Caller must zero-initialize the cache. Invalid views never return a stale
 * matrix; successful repeated views reuse the existing coefficients. */
typedef struct { PcEndingView view; PcEndingAffine matrix; uint8_t valid; } PcEndingAffineCache;
const PcEndingAffine *pc_ending_affine_cached(PcEndingAffineCache *cache,const PcEndingView *view);
/* Prepare ordered planar quad edges for the existing 16-bit fill-seed clipper.
 * Reject nonzero Z or projected coordinates outside +/-8191 atomically.
 * No perspective/64-bit clipping helpers are called on this path. */
int pc_ending_affine_quad(const PcEndingAffine *matrix,const PcEndingVertex vertices[4],
                          int16_t xs[4],int16_t ys[4]);
