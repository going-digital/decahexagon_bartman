#pragma once
#include <exec/types.h>

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
