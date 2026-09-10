#pragma once

// Build configuration
//#define MUSIC
#define MUSIC_LSP
#define ASM_OPT
//#define SHOW_DRAW_PLANE
//#define SKIP_FILL
//#define DEBUG_NAG
#define DEBUG_SPAG

#define MAX_NUM_SIDES (12)
#define NUM_SIDES MAX_NUM_SIDES

#define SCREEN_WIDTH (320) // Currently fixed at 320 due to cls routine
#define SCREEN_HEIGHT (200) // Must be multiple of 4 for cls routine
#define FRAME_RATE (50) // To keep difficulty consistent between PAL and NTSC
#define SCREEN_WIDTH_BYTES (SCREEN_WIDTH >> 3)

// Pixel aspect correction for NTSC
#if FRAME_RATE == 60
// Should be *0.8333
// n - (n>>2) = 0.75
// n - (n>>2) + (n>>4) = 0.8125 close enough

#define PIXEL_ASPECT_CORRECT_Y(n) (n * 5 / 6)
#else
// Strictly this is 15/16, but 1 is close enough
#define PIXEL_ASPECT_CORRECT_Y(n) (n)
#endif

#define BITPLANE_SIZE (SCREEN_HEIGHT * SCREEN_WIDTH_BYTES)

// Viewport clipping bounds and line-slope fixed point
#define XMAX (SCREEN_WIDTH-1)
#define YMAX (SCREEN_HEIGHT-1)
#define FRACBITS 8
