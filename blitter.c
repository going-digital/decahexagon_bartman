#include "blitter.h"

void blit_line_mode(void) {
    blit_wait();
    // Preload registers for line mode activities
    // Saves resetting them every line.
    custom->bltbdat = 0xffff;
    custom->bltafwm_lwm = 0xffffffff;
    custom->bltcmod_bmod = (SCREEN_WIDTH_BYTES << 16) | SCREEN_WIDTH_BYTES;
}

void blit_clipped_line_onedot(
    WORD x0, WORD y0, WORD x1, WORD y1, UWORD angle, void *bitplane
) {
    // Draws from x0/y0 to x1/y1. Inclusive of lowest y, exclusive of largest y.
    WORD outside_viewport = 4;
    WORD viewport_intersection = 0;
    WORD mxy = 0;
    WORD myx = 0;

    // Clip at y=0
    if (y0 > y1) {
        WORD tmp;
        tmp = x0; x0 = x1; x1 = tmp;
        tmp = y0; y0 = y1; y1 = tmp;
    }
    if (y1 < 0) {
        // Entire line is above screen. Discard.
        return;
    } else if (y0 < 0) {
        // Test for intersection with top of screen
        #ifdef ASM_OPT
        mxy = x1 - x0;
        WORD yd = y1 - y0;
        asm(
            "ext.l  %[mxy]\n"
            "asl.l  %[fracbits],%[mxy]\n"
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
        #else
        mxy = ((x1 - x0) << FRACBITS) / (y1 - y0);
        WORD new_x = x0 - ((y0 * mxy) >> FRACBITS);
        #endif

        if (new_x >= 0 && new_x <= XMAX) {
            // Line intersects top of screen. Move x0/y0 point onscreen and flag the intersection.
            x0 = new_x;
            y0 = 0;
            viewport_intersection = 1;
        }
    } else {
        outside_viewport -= 1;
    }

    // Clip at y=YMAX
    if (y0 > YMAX) {
        // Entire line is below screen. Discard.
        return;
    } else if (y1 > YMAX) {
        if (!mxy) {
            #ifdef ASM_OPT
            mxy = x1 - x0;
            WORD yd = y1 - y0;
            asm(
                "ext.l  %[mxy]\n"
                "asl.l  %[fracbits],%[mxy]\n"
                "divs.w %[yd],%[mxy]\n"
                : [mxy]"+&d"(mxy)
                : [yd]"d"(yd), [fracbits]"I"(FRACBITS)
                : "cc"
            );
            #else
            mxy = ((x1 - x0) << FRACBITS) / (y1 - y0);
            #endif
        }

        #ifdef ASM_OPT
        WORD result;
        asm(
            "move.w %[mxy],%[result]\n"
            "muls.w %[y1],%[result]\n"
            "asr.l  %[fracbits],%[result]\n"
            : [result]"=&d"(result)
            : [mxy]"d"(mxy), [y1]"d"(YMAX - y1), [fracbits]"I"(FRACBITS)
            : "cc"
        );
        WORD new_x = x1 + result;
        #else
        WORD new_x = x1 + (((YMAX - y1) * mxy) >> FRACBITS);
        #endif

        if (new_x >= 0 && new_x <= XMAX) {
            // Line intersects bottom of screen. Move x1/y1 point onscreen and flag the intersection.
            x1 = new_x;
            y1 = YMAX;
            viewport_intersection = 1;
        }
    } else {
        outside_viewport -= 1;
    }

    // Clip at x=0
    if (x0 > x1) {
        WORD tmp;
        tmp = x0; x0 = x1; x1 = tmp;
        tmp = y0; y0 = y1; y1 = tmp;
    }
    if (x1 < 0) {
        // Entire line is left of screen. Discard.
        return;
    } else if (x0 < 0) {
        #ifdef ASM_OPT
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
        #else
        myx = ((y1 - y0) << FRACBITS) / (x1 - x0);
        WORD new_y = y0 - ((x0 * myx) >> FRACBITS);
        #endif

        if (new_y >= 0 && new_y <= YMAX) {
            // Line intersects left of screen. Move x0/y0 point onscreen and flag the intersection.
            x0 = 0;
            y0 = new_y;
            viewport_intersection = 1;
        }
    } else {
        outside_viewport -= 1;
    }
    if (x0 > XMAX) {
        // Entire line is right of screen. But still need to get fill state correct.
        blit_fill_fix_onedot(y0, y1, bitplane);
        return;
    } else if (x1 > XMAX) {
        if (!myx) {
            #ifdef ASM_OPT
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
            #else
            myx = ((y1 - y0) << FRACBITS) / (x1 - x0);
            #endif
        }
        #ifdef ASM_OPT
        WORD result;
        asm(
            "move.w %[myx],%[result]\n"
            "muls.w %[x1],%[result]\n"
            "asr.l  %[fracbits],%[result]\n"
            : [result]"=&d"(result)
            : [myx]"d"(myx), [x1]"d"(XMAX - x1), [fracbits]"I"(FRACBITS)
            : "cc"
        );
        WORD new_y = y1 + result;
        #else
        WORD new_y = y1 + (((XMAX - x1) * myx) >> FRACBITS);
        #endif

        if (new_y < 0) {
            // TODO: Is this needed?
            blit_fill_fix_onedot(0, y1, bitplane);
        } else if (new_y > YMAX) {
            // TODO: Is this needed?
            blit_fill_fix_onedot(y1, YMAX, bitplane);
        } else {
            blit_fill_fix_onedot(y1, new_y, bitplane);
            x1 = XMAX;
            y1 = new_y;
            viewport_intersection = 1;
        }
        // TODO: What kind of lines are in the else clause here? Do they also need a fillfix?
        // They cross the x=XMAX line, but not on-screen.
    } else {
        outside_viewport -= 1;
    }
    if (outside_viewport == 0 || viewport_intersection) {
        blit_line_onedot(x0, y0, x1, y1, bitplane);
    }
}

void blit_line_onedot(
    UWORD x0, UWORD y0,
    UWORD x1, UWORD y1,
    void *bitplane
) {
    // Draws a line from x0,y0 to x1,y1.
    // Pixels x0,y0 and x1,y1 are guaranteed to be drawn.
    //
    // See http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0128.html
    //

    // Horizontal lines already have a pixel at start and end from other edges.
    // No drawing required.
    if (y0 == y1) return;

    // Swap end points to draw in a south/easterly direction (Octants 4 5 6 7 only)
    if (y0 > y1) {
        UWORD tmp;
        tmp = y0; y0 = y1; y1 = tmp;
        tmp = x0; x0 = x1; x1 = tmp;
    }

    // Based on https://www.markwrobel.dk/post/amiga-machine-code-letter12-linedraw2/
    // Calculate word address of start point
    // Note octants 0, 1, 2, 3 are omitted as they are never drawn.

    APTR startpt = bitplane + muluw(y0, SCREEN_WIDTH_BYTES) + ((x0 >> 4) << 1);
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
    maj_d <<= 2;
    WORD bltamod = bltbmod - maj_d; // 4 min_d - 4 maj_d
    WORD bltaptl = bltamod; // This goes against HRM, but seems to work well.
    if (bltaptl < 0) bltcon1 |= SIGNFLAG;

    // Set starting word, DMA channels and logic function
    // TODO: Does this skip the first pixel? BC0F_DEST is not set.
    // See https://www.markwrobel.dk/post/amiga-machine-code-letter12-linedraw2/
    // https://eab.abime.net/showpost.php?p=206412&postcount=6
    //
    // FIXME: This appears to draw the first pixel, but does not draw the last pixel.
    UWORD bltcon0 = (
        (x0 & 0xf) << 12 // Starting bit within word
        | BC0F_SRCC | BC0F_SRCA // Missing DEST here
        | ABNC | NABC | NANBC // 4a xor
    );
    // Spin until blitter free
    blit_wait();

    // Set up
    custom->bltadat = 0x8000;
    custom->bltamod = bltamod;
    custom->bltbmod = bltbmod;
    custom->bltapt_l = bltaptl;
    custom->bltcpt = startpt;
    custom->bltdpt = startpt;
    custom->bltcon0 = bltcon0;
    custom->bltcon1 = bltcon1;
    custom->bltsize = (maj_d << 4) + 2;
}

void blit_fill_fix_onedot(
    WORD y0, WORD y1, void *bitplane
) {
    // Draws from ymin (inclusive) to ymax (exclusive)
    if (y0 > y1) {
        WORD tmp;
        tmp = y0; y0 = y1; y1 = tmp;
    }
    // Skip offscreen
    if (y1 < 0) return;
    if (y0 > YMAX) return;
    // Clip to screen
    if (y0 < 0) y0 = 0;
    if (y1 > YMAX) y1 = YMAX;
    // Skip zero length lines
    if (y1 == y0) return;

    APTR startpt = (
        bitplane
        + muluw(y0, SCREEN_WIDTH_BYTES)
        + (SCREEN_WIDTH >> 3) - 2
    ); // Location of rightmost word
    UWORD maj_d = (y1 - y0) << 1;
    WORD bltaptl = -maj_d; // 4 min_d - 2 maj_d
    WORD bltamod = bltaptl - maj_d; // 4 min_d - 4 maj_d
    // Spin until blitter free
    blit_wait();
    // Set up
    custom->bltadat = 0x8000;
    custom->bltamod = bltamod;
    custom->bltbmod = 0;
    custom->bltapt = (APTR)((ULONG)bltaptl);
    custom->bltcpt = startpt;
    custom->bltdpt = startpt;
    custom->bltcon0 = (
        (0xf << 12)             // Rightmost word
        | BC0F_SRCC | BC0F_SRCA // Set DMA channels
        | ABNC | NABC | NANBC   // 4a xor
    );
    custom->bltcon1 = LINEMODE | SIGNFLAG;
    custom->bltsize = (maj_d << 5) + 2; // Remember maj_d was doubled above
}

void blit_line(
    UWORD x0, UWORD y0,
    UWORD x1, UWORD y1,
    void *bitplane
) {
    // TODO: First pixel? Last pixel?

    // Draws a line from x0,y0 to x1,y1.
    // Pixels x0,y0 and x1,y1 are guaranteed to be drawn.
    //
    // See http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0128.html
    //
    if (y0 > y1) {
        UWORD tmp;
        tmp = y0; y0 = y1; y1 = tmp;
        tmp = x0; x0 = x1; x1 = tmp;
    }

    // Based on https://www.markwrobel.dk/post/amiga-machine-code-letter12-linedraw2/
    // Calculate word address of start point
    APTR startpt = (
        bitplane
        + muluw(y0, SCREEN_WIDTH_BYTES)
        + ((x0 >> 4) << 1)
    );
    WORD ed = x1 - x0; // Positive in east direction
    UWORD sd = y1 - y0; // Positive in south direction
    UWORD bltcon1;
    UWORD maj_d;
    UWORD min_d;

    if (ed + sd < 0) {
        // x is major axis
        maj_d = -ed;
        // Octant 4
        min_d = sd;
        bltcon1 = SUD | AUL | LINEMODE;
    } else {
        // Octant 0567 Southeast
        // AUL = 0
        if (ed - sd < 0) {
            // South predominant
            // Octant 5 6
            // SUD = 0
            maj_d = sd;
            if (ed < 0) {
                // Octant 5
                bltcon1 = SUL | LINEMODE;
                min_d = -ed;
            } else {
                // Octant 6
                bltcon1 = LINEMODE;
                min_d = ed;
            }
        } else {
            // East predominant
            maj_d = ed;
            // Octant 7
            bltcon1 = SUD | LINEMODE;
            min_d = sd;
        }
    }
    // After that, majd is pixel distance on dominant axis,
    // mind is pixel distance on minor axis. Both are guaranteed zero/positive.
    // Preshift max_d, min_d
    maj_d <<= 1;
    WORD bltbmod = min_d << 2; // 4min_d
    WORD bltaptl = bltbmod - maj_d; // 4 min_d - 2 maj_d
    WORD bltamod = bltaptl - maj_d; // 4 min_d - 4 maj_d
    if (bltaptl < 0) bltcon1 |= SIGNFLAG;

    // Set starting word
    // Set starting bit
    UWORD bltcon0 = (x0 & 0xf) << 12;
    // Set DMA channels
    bltcon0 |= BC0F_DEST | BC0F_SRCC | BC0F_SRCA | ABC | ABNC | NABC | NANBC;  // or
    //bltcon0 |= BC0F_DEST | BC0F_SRCC | BC0F_SRCA | ABNC | NABC | NANBC;  // xor
    //bltcon0 |= BC0F_DEST | BC0F_SRCC | BC0F_SRCA | ABNC | NABC | NANBC;  // xor
    // Spin until blitter free
    blit_wait();
    // Set up
    custom->bltadat = 0x8000;
    custom->bltamod = bltamod;
    custom->bltbmod = bltbmod;
    custom->bltapt = (APTR)((ULONG)bltaptl);
    custom->bltcpt = startpt;
    custom->bltdpt = startpt;
    custom->bltcon0 = bltcon0;
    custom->bltcon1 = bltcon1;
    custom->bltsize = (maj_d << 5) + ((1 << 6) + 2); // Remember maj_d was doubled above
}

void blit_cls(void *bitplane) {
    blit_wait();
    custom->bltcon0 = BC0F_DEST;
    custom->bltcon1 = 0;
    //custom->bltafwm = 0xffff; Not required because propogated from line mode
    //custom->bltalwm = 0xffff; Not required because propogated from line mode
    custom->bltdpt = bitplane;
    custom->bltdmod = 0;
    custom->bltsize = (SCREEN_HEIGHT << 6) | (SCREEN_WIDTH_BYTES >> 1);
}

void cpu_cls(void *bitplane) {
    register volatile const UWORD _d0 ASM("d0") = ((SCREEN_HEIGHT * SCREEN_WIDTH_BYTES) / 160) - 1;
    // This assumes 320 pixels wide and HEIGHT is a multiple of 4 lines
    register volatile const void* _a0 ASM("a0") = bitplane + SCREEN_HEIGHT * SCREEN_WIDTH_BYTES;
    __asm volatile (
        "   movem.l %%d1-%%d7/%%a1-%%a3,-(%%sp)\n"
        "   moveq.l #0,%%d1\n"
        "   moveq.l #0,%%d2\n"
        "   moveq.l #0,%%d3\n"
        "   moveq.l #0,%%d4\n"
        "   moveq.l #0,%%d5\n"
        "   moveq.l #0,%%d6\n"
        "   moveq.l #0,%%d7\n"
        "   movea.l %%d1,%%a1\n"
        "   movea.l %%d1,%%a2\n"
        "   movea.l %%d1,%%a3\n"
        "1: movem.l %%d1-%%d7/%%a1-%%a3,-(%%a0)\n"// 40 bytes
        "   movem.l %%d1-%%d7/%%a1-%%a3,-(%%a0)\n"// 40 bytes
        "   movem.l %%d1-%%d7/%%a1-%%a3,-(%%a0)\n"// 40 bytes
        "   movem.l %%d1-%%d7/%%a1-%%a3,-(%%a0)\n"// 40 bytes
        "   dbra    %%d0,1b\n"
        "   movem.l (%%sp)+,%%d1-%%d7/%%a1-%%a3\n"
        :
        : "rf"(_d0), "rf"(_a0)
        : "cc", "memory"
    );
}

void blit_fill(void *bitplane, void *bitplane2) {
    APTR start = bitplane + SCREEN_HEIGHT * SCREEN_WIDTH_BYTES - 2;
    APTR start2 = bitplane2 + SCREEN_HEIGHT * SCREEN_WIDTH_BYTES - 2;
    blit_wait();
    custom->bltcon0 = BC0F_SRCA | BC0F_DEST | A_TO_D;
    custom->bltcon1 = FILL_XOR | BLITREVERSE;
    custom->bltafwm = 0xffff;
    custom->bltalwm = 0xffff;
    custom->bltapt = start;
    custom->bltdpt = start2;
    custom->bltamod = 0;
    custom->bltdmod = 0;
    custom->bltsize = (SCREEN_HEIGHT << 6) | (SCREEN_WIDTH_BYTES >> 1);
}
