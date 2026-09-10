#pragma once

// Experimental variant of the blitter primitives that emits blitter setup
// into a copper list instead of poking the registers directly. Currently
// unused by the game loop (kept for the copperlist renderer experiment) and
// garbage-collected out of the final binary while nothing calls it.

#include "hw.h"
#include "config.h"
#include "coplist.h"

WORD* cl_cls(WORD* bitplane, WORD* copPtr);
WORD* cl_line_mode(WORD* copPtr);
WORD* cl_line_onedot(UWORD x0, UWORD y0, UWORD x1, UWORD y1, void *bitplane, WORD* copPtr);
WORD* cl_fill(void *bitplane, void *bitplane2, WORD* copPtr);
WORD* cl_fill_fix_onedot(WORD y0, WORD y1, void *bitplane, WORD* copPtr);
WORD* cl_clipped_line_onedot(WORD x0, WORD y0, WORD x1, WORD y1, UWORD angle, void *bitplane, WORD* copPtr);
