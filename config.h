#pragma once

// ---- build target ---------------------------------------------------------
// BUILD_DEBUG: profiling raster bar, KPrintF, WinUAE debug-overlay hooks,
//              warpmode() around precalc, the startup banner.
// Override from the Makefile with -DBUILD_DEBUG=0 for a release build.
#ifndef BUILD_DEBUG
#define BUILD_DEBUG 1
#endif

// TARGET_NTSC: 60Hz timing + pixel-aspect correction. Default is 50Hz PAL.
//#define TARGET_NTSC

#ifdef TARGET_NTSC
#define FRAME_RATE (60)
#else
#define FRAME_RATE (50) // Keeps difficulty consistent between PAL and NTSC
#endif

// Startup banner text (BUILD_DEBUG only). Pick one.
#define DEBUG_NAG
//#define DEBUG_NGD
//#define DEBUG_SPAG

// ---- renderer tuning ----------------------------------------------------
#define ASM_OPT           // hand-tuned m68k asm in the hot clip/line paths
//#define MUSIC            // ThePlayer 6.1a (VBL timed)
#define MUSIC_LSP          // LightSpeed Player (CIA timed)
//#define SHOW_DRAW_PLANE  // show the work buffer as a 2nd bitplane
//#define SKIP_FILL        // skip the area fill (wireframe)

// Hexagon is the max/starting side count; the field morphs down to fewer
// sides (pentagon, square) as a run's difficulty ramps - see game.c's level
// table. MAX_NUM_SIDES only sizes fixed-capacity buffers now; the current
// side count is gamestate.num_sides (runtime, changes mid-run).
#define MAX_NUM_SIDES (6)

#define SCREEN_WIDTH (320) // Currently fixed at 320 due to cls routine
#define SCREEN_HEIGHT (200) // Must be multiple of 4 for cls routine
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

// Raw hardware coordinates (DIWSTRT space) of the visible display's top-left
// corner. screenScanDefault() uses these for DIWSTRT/DIWSTOP; the sprite HUD
// shares them so sprites line up with the bitplane display.
#define DISPLAY_HW_X (129 + (SCREEN_WIDTH - 320) / 2)
#define DISPLAY_HW_Y (44 + (SCREEN_WIDTH - 256) / 2)

// Viewport clipping bounds and line-slope fixed point
#define XMAX (SCREEN_WIDTH-1)
#define YMAX (SCREEN_HEIGHT-1)
#define FRACBITS 8
