#pragma once

// Draws the current game frame (hub, walls, player) into `buf` as one-dot
// fill seeds. The caller runs blit_fill() afterwards and owns buffer flipping.
void render_game(void* buf);

// Radial slot-boundary lines, drawn solid (OR mode) straight into `buf`.
// Call AFTER blit_fill has finished - these are an overlay, not fill seeds.
// Must follow render_game() in the same frame (reuses its zoom).
void render_spokes(void* buf);
