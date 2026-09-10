#include "blitter.h"

void blit_line_mode(void) {
    blit_wait();
    // Preload registers for line mode activities
    // Saves resetting them every line.
    custom->bltbdat = 0xffff;
    custom->bltafwm_lwm = 0xffffffff;
    custom->bltcmod_bmod = (SCREEN_WIDTH_BYTES << 16) | SCREEN_WIDTH_BYTES;
}

// --- viewport-clip helpers -----------------------------------------------
// FRACBITS fixed point. Under ASM_OPT these keep the exact 32-bit
// intermediate / 16-bit result behaviour the clipper was tuned against
// (ext.l -> asl.l -> divs.w, and move.w -> muls.w -> asr.l).

// slope = (num << FRACBITS) / den
static WORD clip_slope(WORD num, WORD den) {
#ifdef ASM_OPT
    WORD r = num;
    asm(
        "ext.l  %[r]\n"
        "asl.l  %[fb],%[r]\n"
        "divs.w %[den],%[r]\n"
        : [r]"+&d"(r)
        : [den]"d"(den), [fb]"I"(FRACBITS)
        : "cc"
    );
    return r;
#else
    return ((LONG)num << FRACBITS) / den;
#endif
}

// (slope * dist) >> FRACBITS
static WORD clip_step(WORD slope, WORD dist) {
#ifdef ASM_OPT
    WORD r;
    asm(
        "move.w %[s],%[r]\n"
        "muls.w %[d],%[r]\n"
        "asr.l  %[fb],%[r]\n"
        : [r]"=&d"(r)
        : [s]"d"(slope), [d]"d"(dist), [fb]"I"(FRACBITS)
        : "cc"
    );
    return r;
#else
    return (slope * dist) >> FRACBITS;
#endif
}

// Clip x0,y0 -> x1,y1 to the 0..XMAX / 0..YMAX viewport and, if anything
// survives, hand it to blit_line_onedot as XOR fill seeds. Where the line
// leaves the right edge, the off-screen remainder is still walked down the
// x=XMAX column by blit_fill_fix_onedot so the fill parity stays correct.
// Mirrors clipline() in cliptest.py. `angle` is reserved for angled fills.
void blit_clipped_line_onedot(
    WORD x0, WORD y0, WORD x1, WORD y1, UWORD angle, void *bitplane
) {
    (void)angle;
    WORD outside_viewport = 4;
    WORD viewport_intersection = 0;
    WORD mxy = 0; // dx/dy, lazily computed and shared by the two y edges
    WORD myx = 0; // dy/dx, lazily computed and shared by the two x edges

    // Order endpoints by y for the top/bottom tests.
    if (y0 > y1) {
        WORD t;
        t = x0; x0 = x1; x1 = t;
        t = y0; y0 = y1; y1 = t;
    }

    // Top edge (y = 0)
    if (y1 < 0) {
        return; // wholly above the viewport
    } else if (y0 < 0) {
        mxy = clip_slope(x1 - x0, y1 - y0);
        WORD nx = x0 - clip_step(mxy, y0);
        if (nx >= 0 && nx <= XMAX) {
            x0 = nx;
            y0 = 0;
            viewport_intersection = 1;
        }
    } else {
        outside_viewport--;
    }

    // Bottom edge (y = YMAX)
    if (y0 > YMAX) {
        return; // wholly below the viewport
    } else if (y1 > YMAX) {
        if (!mxy) mxy = clip_slope(x1 - x0, y1 - y0);
        WORD nx = x1 + clip_step(mxy, YMAX - y1);
        if (nx >= 0 && nx <= XMAX) {
            x1 = nx;
            y1 = YMAX;
            viewport_intersection = 1;
        }
    } else {
        outside_viewport--;
    }

    // Order endpoints by x for the left/right tests.
    if (x0 > x1) {
        WORD t;
        t = x0; x0 = x1; x1 = t;
        t = y0; y0 = y1; y1 = t;
    }

    // Left edge (x = 0)
    if (x1 < 0) {
        return; // wholly left of the viewport
    } else if (x0 < 0) {
        myx = clip_slope(y1 - y0, x1 - x0);
        WORD ny = y0 - clip_step(myx, x0);
        if (ny >= 0 && ny <= YMAX) {
            x0 = 0;
            y0 = ny;
            viewport_intersection = 1;
        }
    } else {
        outside_viewport--;
    }

    // Right edge (x = XMAX). Done last: the part beyond XMAX still needs
    // fill-fixup toggles down the x=XMAX column.
    if (x0 > XMAX) {
        // Nothing visible, but every scanline it spans still toggles the fill.
        blit_fill_fix_onedot(y0, y1, bitplane);
        return;
    } else if (x1 > XMAX) {
        if (!myx) myx = clip_slope(y1 - y0, x1 - x0);
        WORD ny = y1 + clip_step(myx, XMAX - x1);
        // Walk the beyond-XMAX part down the right column. blit_fill_fix_onedot
        // clamps the y range to the screen, so ny < 0 / ny > YMAX are covered
        // here too (this is what the old three-way branch was reaching for).
        blit_fill_fix_onedot(y1, ny, bitplane);
        if (ny >= 0 && ny <= YMAX) {
            x1 = XMAX;
            y1 = ny;
            viewport_intersection = 1;
        }
    } else {
        outside_viewport--;
    }

    if (outside_viewport == 0 || viewport_intersection) {
        blit_line_onedot(x0, y0, x1, y1, bitplane);
    }
}

// Plots ONE toggle pixel per scanline along x0,y0 -> x1,y1 to seed the XOR
// area fill (blit_fill). The y span is half-open: scanlines
// [min(y0,y1), max(y0,y1)) get a pixel and the max-y endpoint does not, so a
// vertex shared by two edges toggles exactly once.
// Requires blit_line_mode() earlier this frame.
// Line-mode setup after https://www.markwrobel.dk/post/amiga-machine-code-letter12-linedraw2/
// See http://amigadev.elowar.com/read/ADCD_2.1/Hardware_Manual_guide/node0128.html
void blit_line_onedot(
    UWORD x0, UWORD y0,
    UWORD x1, UWORD y1,
    void *bitplane
) {
    // Horizontal segments contribute no scanline crossings to the fill.
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

    // Start word/bit, DMA channels and minterm (4a XOR). BC0F_DEST is left
    // clear deliberately: this matches the OCS-tuned setup, and bltsize height
    // below is maj_d (not maj_d + 1), which drops the max-y pixel and gives the
    // half-open y span the fill relies on.
    // https://eab.abime.net/showpost.php?p=206412&postcount=6
    UWORD bltcon0 = (
        (x0 & 0xf) << 12 // Starting bit within word
        | BC0F_SRCC | BC0F_SRCA
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
