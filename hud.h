#pragma once

#include <exec/types.h>

// Sprite-based HUD + title screen. Composited by Denise independently of the
// bitplane/blitter pipeline, so it costs nothing on the render frame budget.
// OCS gives 8 sprite channels of 16px each, so this is good for a short
// fixed-width readout (a timer) or a one-line title (all 8 channels, during
// MODE_ATTRACT) - not prose. The title and the timer HUD are mutually
// exclusive (both want most/all of the 8 channels), see hud_emit_copper.
//
// SPRxPT is only ever written by the COPPER (hud_emit_copper), never the CPU,
// even though the content it points at is fixed at hud_init() time and never
// changes. A CPU move.l to that register lands as two separate 16-bit bus
// writes, and Agnus reads the same register every vblank to fetch that
// sprite's pos/ctl - hitting it mid-write yields a torn pointer and garbage
// on screen. The copper's MOVE runs inside the same DMA arbitration Agnus
// uses, so it can't race it the way the CPU can.
void hud_init(void);                     // build the glyph/title buffers, set static sprite colours
void hud_tick(void);                     // pick this frame's HUD glyphs (pure logic, no hardware writes)
USHORT* hud_emit_copper(USHORT* copPtr); // append this frame's SPRxPT copper writes, return the new end
UBYTE hud_flash_now(void);               // 1 for a couple of ticks as the title cuts away to the HUD
