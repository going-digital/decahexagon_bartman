#pragma once

#include <exec/types.h>

// Sprite-based HUD + title screen. Composited by Denise independently of the
// bitplane/blitter pipeline; CPU glyph selection and Copper setup still cost time.
// OCS gives 8 sprite channels of 16px each, so this is good for a short
// fixed-width readout (a timer) and a one-line title (all 8 channels).
// The copper reuses the channels below the timer so MODE_ATTRACT displays
// both the selected profile's best and its menu text, see hud_emit_copper.
// After the banner row, channels 6/7 are reloaded with this frame's player.
//
// SPRxPT is only ever written by the COPPER (hud_emit_copper), never the CPU,
// HUD descriptors are fixed at hud_init(); the player uses three buffers.
// A CPU move.l to that register lands as two separate 16-bit bus
// writes, and Agnus reads the same register every vblank to fetch that
// sprite's pos/ctl - hitting it mid-write yields a torn pointer and garbage
// on screen. The copper's MOVE runs inside the same DMA arbitration Agnus
// uses, so it can't race it the way the CPU can.
void hud_free(void); // call after restoring OS DMA/copper
void hud_init(void);                     // build the glyph/title buffers, set static sprite colours
void hud_tick(void);                     // pick this frame's HUD glyphs (pure logic, no hardware writes)
USHORT* hud_emit_copper(USHORT* copPtr); // append this frame's SPRxPT copper writes, return the new end
UBYTE hud_flash_now(void);               // 1 for a couple of ticks as the title cuts away to the HUD

/* Static loading list for the native blocking loader. */
UWORD* hud_loading_copper(void *plane,UBYTE saving);
void hud_loading_begin(void);
void hud_loading_end(void);

/* 0 clear, 1 save error, 2 disk write protected. */
void hud_save_failed(UBYTE failed);

void hud_draw_completion(void *plane);
