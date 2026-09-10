#pragma once

// Draws the current game frame (hub, walls, player) into `buf` as one-dot
// fill seeds. The caller runs blit_fill() afterwards and owns buffer flipping.
void render_game(void* buf);
