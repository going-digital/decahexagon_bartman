#pragma once
#include <exec/types.h>
#include "pc_world.h"

// The HUD releases channels 6/7 after its lower banner row plus DMA guard.
#define PLAYER_SPRITE_RELOAD_Y 34
#define PLAYER_SPRITE_MIN_Y (PLAYER_SPRITE_RELOAD_Y + 2)

// Precompute viewport endpoints after init_tables(), before rendering.
void render_init(void);
void render_free(void); // after restoring OS DMA/copper

// Draws the current game frame (hub and walls) into `buf` as one-dot
// fill seeds. The caller runs blit_fill() afterwards and owns buffer flipping.
void render_game(void* buf);

// Radial slot-boundary lines, drawn solid (OR mode) straight into `buf`.
// Call AFTER blit_fill has finished - these are an overlay, not fill seeds.
// Must follow render_game() in the same frame (reuses its zoom).
void render_spokes(void* buf);

// Prepare the next pair of player sprites; call after spokes on the same buf.
// Unusual positions overlapping the HUD or viewport use a solid CPU overlay.
void render_player(void* buf);
const UWORD* render_player_sprite(unsigned column);
// Pixel data can come from an immutable gameplay pose or the live buffer.
const UWORD* render_player_pixels(unsigned column);

/* Shared flat scene input. Geometry fields use the same units as GameState.
 * Call before render_spokes/render_player; those consume this frame snapshot.
 * A null world draws the hub/player scene without walls. */
typedef struct {
    UWORD field_angle,segment_angle,player_angle,draw_distance,pulse;
    UBYTE num_sides;
    WORD shake_x,shake_y;
} RenderScene;
void render_scene(void *buffer,const RenderScene *scene,const PcWorld *world);
