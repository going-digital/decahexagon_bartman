#pragma once

// Blitter/CPU primitives that render into a 1-bitplane buffer:
// clear, one-dot clipped line draw and XOR area fill.

#include "hw.h"
#include "config.h"

void blit_line_mode(void);

void blit_cls(void *bitplane);
void cpu_cls(void *bitplane);

void blit_line_onedot(UWORD x0, UWORD y0, UWORD x1, UWORD y1, void *bitplane);
void blit_line(UWORD x0, UWORD y0, UWORD x1, UWORD y1, void *bitplane);

void blit_fill(void *bitplane, void *bitplane2);
void blit_fill_fix_onedot(WORD y0, WORD y1, void *bitplane);
void blit_fill_reset(void); // call before the frame's seeds so blit_fill can bound its work

void blit_clipped_line_onedot(
    WORD x0, WORD y0, WORD x1, WORD y1, UWORD angle, void *bitplane
);

__attribute__((always_inline)) inline void blit_wait(void) {
    UWORD dummy = custom->dmaconr; // Dummy read for thin Agnus compatibility
    (void)dummy;
    while (custom->dmaconr & DMAF_BLTDONE);
}
