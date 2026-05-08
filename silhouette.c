#include "silhouette.h"

#define SCREEN_WIDTH_BYTES (SCREEN_WIDTH >> 3)

#define XMAX (SCREEN_WIDTH-1)
#define YMAX (SCREEN_HEIGHT-1)

static void * copperlist_ptr;
static UWORD * bitplane;

extern void sil_set_render_plane(UWORD * bitplane_ptr) {
    bitplane = bitplane_ptr;
}

extern void sil_set_copperlist(void *copperList) {
    copperlist_ptr = copperList;
}

// Pause the copperlist until the blitter completes
__attribute__((always_inline)) inline
void sil_wait_blit() {
    *(ULONG *)copperlist_ptr++ = 0xffff0000;
}

// Write to 16 bit register, optimised for fixed values
__attribute__((always_inline)) inline
void sil_write_reg_const(const UWORD addr, const UWORD value) {
    *(ULONG *)copperlist_ptr++ = (addr << 16) | value;
}

// Write to 32 bit register, optimised for fixed values
__attribute__((always_inline)) inline
void sil_write_reg32_const(const UWORD addr, const ULONG value) {
    *(ULONG *)copperlist_ptr++ = (addr << 16) | (value >> 16);
    *(ULONG *)copperlist_ptr++ = ((addr + 2) << 16) | (value & 0xffff);
}

// Write to 16 bit register, optimised for dynamic values
__attribute__((always_inline)) inline
void sil_write_reg(const UWORD addr, UWORD value) {
    *(UWORD *)copperlist_ptr++ = addr;
    *(UWORD *)copperlist_ptr++ = value;
}

// Write to 32 bit register, optimised for dynamic values
__attribute__((always_inline)) inline
void sil_write_reg32(const UWORD addr, ULONG value) {
    *(UWORD *)copperlist_ptr++ = addr;
    *(UWORD *)copperlist_ptr++ = value >> 16;
    *(UWORD *)copperlist_ptr++ = addr + 2;
    *(UWORD *)copperlist_ptr++ = value & 0xffff;
}

// Clear complete bitplane
void sil_cls() {
    sil_wait_blit();
    sil_write_reg_const(offsetof(struct Custom, bltcon0), BC0F_DEST);
    sil_write_reg_const(offsetof(struct Custom, bltcon1), 0);
    sil_write_reg_const(offsetof(struct Custom, bltdmod), 0);
    sil_write_reg32(offsetof(struct Custom, bltdpt), bitplane);
    sil_write_reg_const(offsetof(struct Custom, bltsize), (SCREEN_HEIGHT << 6) | (SCREEN_WIDTH_BYTES >> 1));
}

void sil_fill() {
    sil_wait_blit();
    sil_write_reg_const(offsetof(struct Custom, bltcon0), BC0F_SRCA | BC0F_DEST | A_TO_D);
    sil_write_reg_const(offsetof(struct Custom, bltcon1), FILL_XOR | BLITREVERSE);
    sil_write_reg_const(offsetof(struct Custom, bltafwm), 0xffff);
    sil_write_reg_const(offsetof(struct Custom, bltalwm), 0xffff);
    sil_write_reg_const(offsetof(struct Custom, bltamod), 0);
    sil_write_reg_const(offsetof(struct Custom, bltdmod), 0);
    APTR bitplane_start = bitplane + SCREEN_HEIGHT * SCREEN_WIDTH_BYTES - 2;
    sil_write_reg32(offsetof(struct Custom, bltapt), bitplane_start);
    sil_write_reg32(offsetof(struct Custom, bltdpt), bitplane_start);
    sil_write_reg_const(offsetof(struct Custom, bltsize), (SCREEN_HEIGHT << 6) | (SCREEN_WIDTH_BYTES >> 1));
}

void sil_line_mode() {
    sil_wait_blit();
    sil_write_reg_const(offsetof(struct Custom, bltbdat), 0xffff);
    sil_write_reg_const(offsetof(struct Custom, bltafwm), 0xffff);
    sil_write_reg_const(offsetof(struct Custom, bltalwm), 0xffff);
    sil_write_reg_const(offsetof(struct Custom, bltbmod), SCREEN_WIDTH_BYTES);
    sil_write_reg_const(offsetof(struct Custom, bltcmod), SCREEN_WIDTH_BYTES);
}

#define FRACBITS 8

void sil_clipped_line_onedot(WORD x0, WORD y0, WORD x1, WORD y1, UWORD angle) {
    WORD outside_viewport = 4;
    WORD viewport_intersection = 0;
    WORD mxy = 0;
    WORD myx = 0;
    if (y0 == y1) {
        // Line is horizontal - skip it completely
        return;
    } else if (y0 > y1) {
        // Flip points so y0 < y1
        WORD tmp;
        tmp = x0; x0 = x1; x1 = tmp;
        tmp = y0; y0 = y1; y1 = tmp;
    }
    
    // XXX: At this point y0 <= y1

    // Handle y=0 boundary
    if (y1 < 0) {
        // Line is offscreen beyond y=0
        return;
    } else if (y0 < 0) {
        // Line crosses y = 0
        // Calculate intersection point at y=0
        mxy = x1 - x0;
        WORD yd = y1 - y0;
        asm(
            "ext.l %[mxy]\n"
            "asl.l %[fracbits],%[mxy]\n"
            "divs.w %[yd],%[mxy]\n"
            : [mxy]"+&d"(mxy)
            : [yd]"d"(yd), [fracbits]"I"(FRACBITS)
            : "cc"
        );
        WORD result;
        asm(
            "move.w %[mxy],%[result]\n"
            "muls.w %[y0],%[result]\n"
            "asr.l  %[fracbits],%[result]\n"
            : [result]"=&d"(result)
            : [mxy]"d"(mxy), [y0]"d"(y0), [fracbits]"I"(FRACBITS)
            : "cc"
        );
        WORD new_x = x0 - result;
        if (new_x >= 0 && new_x <= XMAX) {
            x0 = new_x;
            y0 = 0;
            viewport_intersection = 1;
        }
    } else {
        // Line is greater than y=0
        outside_viewport -= 1;
    }

    // Handle y=YMAX boundary
    if (y0 > YMAX) {
        // Line is offscreen beyond y=YMAX
        return;
    } else if (y1 > YMAX) {
        // Line crosses y = YMAX
        // Calculate intersection point at y=YMAX
        if (!mxy) {
            mxy = x1 - x0;
            WORD yd = y1 - y0;
            asm(
                "ext.l %[mxy]\n"
                "asl.l %[fracbits],%[mxy]\n"
                "divs.w %[yd],%[mxy]\n"
                : [mxy]"+&d"(mxy)
                : [yd]"d"(yd), [fracbits]"I"(FRACBITS)
                : "cc"
            );
        }
        WORD result;
        asm(
            "move.w %[mxy],%[result]\n"
            "muls.w %[y1],%[result]\n"
            "asr.l %[fracbits],%[result]\n"
            : [result]"=&d"(result)
            : [mxy]"d"(mxy), [y1]"d"(YMAX - y1), [fracbits]"I"(FRACBITS)
            : "cc"
        );
        WORD new_x = x1 + result;

        if (new_x >= 0 && new_x <= XMAX) {
            x1 = new_x;
            y1 = YMAX;
            viewport_intersection = 1;
        }
    } else {
        outside_viewport -= 1;
    }

    if (x0 > x1) {
        WORD tmp;
        tmp = x0; x0 = x1; x1 = tmp;
        tmp = y0; y0 = y1; y1 = tmp;
    }
    // XXX: At this point unknown which y is greater

    // Handle x=0 boundary
    if (x1 < 0) {
        // Line is offscreen beyond x=0
        return;
    } else if (x0 < 0) {
        // Line crosses x = 0
        // Calculate intersection point at x=0

        myx = y1 - y0;
        WORD xd = x1 - x0;
        asm(
            "ext.l  %[myx]\n"
            "asl.l  %[fracbits],%[myx]\n"
            "divs.w %[xd],%[myx]\n"
            : [myx]"+&d"(myx)
            : [xd]"d"(xd), [fracbits]"I"(FRACBITS)
            : "cc"
        );
        WORD result;
        asm(
            "move.w %[myx],%[result]\n"
            "muls.w %[x0],%[result]\n"
            "asr.l  %[fracbits],%[result]\n"
            : [result]"=&d"(result)
            : [myx]"d"(myx), [x0]"d"(x0), [fracbits]"I"(FRACBITS)
            : "cc"
        );
        WORD new_y = y0 - result;
        
        if (new_y >= 0 && new_y <= YMAX) {
            // Line intersects left of screen. Move x0/y0 point onscreen and flag the intersection.
            x0 = 0;
            y0 = new_y;
            viewport_intersection = 1;
        }
    } else {
        outside_viewport -= 1;
    }

    // Handle x=XMAX boundary
    // Any lines offscreen in this direction must be replaced by on-screen lines to correct the polygon fill state.
    if (x0 > XMAX) {
        // Entire line is right of screen. But still need to get fill state correct.
        sil_fill_fix(y0, y1);
        return;
    } else if (x1 > XMAX) {
        if (!myx) {
            myx = y1 - y0;
            WORD xd = x1 - x0;
            asm(
                "ext.l  %[myx]\n"
                "asl.l  %[fracbits],%[myx]\n"
                "divs.w %[xd],%[myx]\n"
                : [myx]"+&d"(myx)
                : [xd]"d"(xd), [fracbits]"I"(FRACBITS)
                : "cc"
            );
        }
        WORD result;
        asm(
            "move.w %[myx],%[result]\n"
            "muls.w %[x1],%[result]\n"
            "asr.l  %[fracbits],%[result]\n"
            : [result]"=&d"(result)
            : [myx]"d"(myx), [x1]"d"(XMAX - x1), [fracbits]"I"(FRACBITS)
            : "cc"
        );
        // TODO: Rewrite this a bit
        if (new_y < 0) {
            sil_fill_fix(0, y1);
        } else if (new_y > YMAX) {
            sil_fill_fix(y1, YMAX);
        } else {
            // TODO: Is y1 < new_y?
            sil_fill_fix(y1, new_y);
            x1 = XMAX;
            y1 = new_y;
            viewport_intersection = 1;
        }
    } else {
        outside_viewport -= 1;
    }
    if (outside_viewport == 0 || viewport_intersection) {
        sil_line_onedot(x0, y0, x1, y1);
    }
}

__attribute__((always_inline)) inline
void sil_fill_fix(WORD y0, WORD y1) {
    if (y0 > y1) {
        // TODO: Does this ever happen?
        WORD tmp;
        tmp = y0; y0 = y1; y1 = tmp;
    }
    // TODO: Do we ever reach these bounds checks?
    if (y1 < 0 || y0 > YMAX) return;
    if (y0 < 0) y0 = 0;
    if (y1 > YMAX) y1 = YMAX;
    if (y1 == y0) return;
    sil_wait_blit();
    sil_write_reg_const(offsetof(struct Custom, bltadat), 0x8000);
    sil_write_reg_const(offsetof(struct Custom, bltbmod), 0);
    sil_write_reg_const(offsetof(struct Custom, bltcon0), (0xf << 12) | BC0F_SRCC | BC0F_SRCA | ABNC | NABC | NANBC);
    sil_write_reg_const(offsetof(struct Custom, bltcon1), LINEMODE | SIGNFLAG);
    APTR startpt = bitplane + muluw(y0, SCREEN_WIDTH_BYTES) + SCREEN_WIDTH_BYTES - 2;
    sil_write_reg32(offsetof(struct Custom, bltcpt), startpt);
    sil_write_reg32(offsetof(struct Custom, bltdpt), startpt);
    UWORD maj_d = (y1 - y0) << 1;
    WORD bltaptl = -maj_d;
    sil_write_reg(offsetof(struct Custom, bltapt), bltaptl);
    WORD bltamod = bltaptl - maj_d;
    sil_write_reg(offsetof(struct Custom, bltamod), bltamod);
    sil_write_reg(offsetof(struct Custom, bltsize), (maj_d << 5) + 2);
}

__attribute__((always_inline)) inline
void sil_line_onedot(WORD x0, WORD y0, WORD x1, WORD y1) {
    if (y0 == y1) {
        return;
    } else if (y0 > y1) {
        UWORD tmp;
        tmp = y0; y0 = y1; y1 = tmp;
        tmp = x0; x0 = x1; x1 = tmp;
    }
    sil_wait_blit();
    sil_write_reg_const(offsetof(struct Custom, bltadat), 0x8000);
    APTR startpt = bitplane + muluw(y0, SCREEN_WIDTH_BYTES) + ((x0 >> 4) < 1);
    sil_write_reg32(offsetof(struct Custom, bltcpt), startpt);
    sil_write_reg32(offsetof(struct Custom, bltdpt), startpt);
    WORD ed = x1 - x0; // Positive in east direction
    UWORD sd = y1 - y0; // Positive in south direction, guaranteed to be positive
    UWORD bltcon1;
    UWORD maj_d;
    UWORD min_d;
    if (ed + sd < 0) {
        // Octant 4
        maj_d = -ed;
        min_d = sd;
        bltcon1 = SUD | AUL | ONEDOT | LINEMODE;
    } else {
        // Octant 0567 Southeast
        if (ed - sd < 0) {
            // South predominant
            maj_d = sd;
            if (ed < 0) {
                // Octant 5
                min_d = -ed;
                bltcon1 = SUL | LINEMODE; // ONEDOT doesn't actually do anything for this octant
            } else {
                // Octant 6
                min_d = ed;
                bltcon1 = LINEMODE; // ONEDOT doesn't actually do anything for this octant
            }
        } else {
            // East predominant
            // Octant 7
            maj_d = ed;
            min_d = sd;
            bltcon1 = SUD | ONEDOT | LINEMODE;
        }
    }
    // After that, majd is pixel distance on dominant axis,
    // mind is pixel distance on minor axis. Both are guaranteed zero/positive.
    // Preshift max_d, min_d
    WORD bltbmod = min_d << 2; // 4min_d
    sil_write_reg(offsetof(struct Custom, bltbmod), bltbmod);
    maj_d <<= 2;
    WORD bltamod = bltbmod - maj_d; // 4 min_d - 4 maj_d
    sil_write_reg(offsetof(struct Custom, bltamod), bltamod);
    sil_write_reg(offsetof(struct Custom, bltapt_l), bltamod);
    if (bltamod < 0) bltcon1 |= SIGNFLAG;
    sil_write_reg(offsetof(struct Custom, bltcon1), bltcon1);
    sil_write_reg(offsetof(struct Custom, bltcon0), (
        (x0 & 0xf) << 12 // Starting bit within word
        | BC0F_SRCC | BC0F_SRCA // Missing DEST here
        | ABNC | NABC | NANBC // 4a xor
    ));
    sil_write_reg(offsetof(struct Custom, bltsize), (maj_d << 4) + 2);
}
