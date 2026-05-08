#define COPBLIT // Render to copperlist

#define SCREEN_WIDTH (320)
#define SCREEN_HEIGHT (200)


__attribute__((always_inline)) inline void cblit_cls(void *bitplane);
__attribute__((always_inline)) inline void cblit_line_onedot(void *bitplane);

#include "support/gcc8_c_support.h"
#include "silhouette.h"
#include <exec/execbase.h>
#include "custom2.h"
#include "hardware/blit.h"

#ifndef _SILHOUETTE_H
#define _SILHOUETTE_H

#define SCREEN_WIDTH (320)
#define SCREEN_HEIGHT (200)

// Set base address of bitplane
void sil_set_render_plane(UWORD * bitplane);

// Set base address of copperlist
void sil_set_copperlist(void *copperList);

// Add copperlist instruction to wait for blitter completion
void sil_wait_blit();
void sil_write_reg_const(const UWORD addr, const UWORD value);
void sil_write_reg32_const(const UWORD addr, const ULONG value);
void sil_write_reg(const UWORD addr, UWORD value);
void sil_write_reg32(const UWORD addr, ULONG value);
void sil_cls();
void sil_fill();
void sil_line_mode();
void sil_clipped_line_onedot(WORD x0, WORD y0, WORD x1, WORD y1, UWORD angle) {
void sil_fill_fix(WORD y0, WORD y1);
void sil_line_onedot(WORD x0, WORD y0, WORD x1, WORD y1);

#endif // _SILHOUETTE_H