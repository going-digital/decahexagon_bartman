#include "hud.h"
#include "config.h"
#include "system.h"  // custom, muluw, AllocMem
#include "coplist.h" // copWritePtr
#include "game.h"

// --- layout --------------------------------------------------------------
#define SPRITE_CHANNELS 8 // total OCS sprite DMA channels
#define HUD_SLOTS    6  // "SSS.CC" - one sprite channel per character
#define HUD_GLYPH_W  4  // glyph width before doubling, in screen (lores) px
#define HUD_GLYPH_H  6  // glyph height, in scanlines
#define HUD_BORDER   1  // white margin around the glyph, each side, before doubling
#define HUD_CELL_W   (HUD_GLYPH_W + 2 * HUD_BORDER)      // 6: opaque tile width
#define HUD_CELL_H   (HUD_GLYPH_H + 2 * HUD_BORDER)      // 8: opaque tile height
#define HUD_CELL_MASK ((1u << HUD_CELL_W) - 1)
#define HUD_SHIFT    (16 - 2 * HUD_CELL_W)               // left-align the doubled cell in the sprite word
#define HUD_X        4  // screen px from the left of the visible display
#define HUD_Y        4  // screen px from the top of the visible display

#define GLYPH_COUNT  12
#define GLYPH_COLON  10
#define GLYPH_PERIOD 11

// Slot content: [tens-of-100s][tens][units][.][tenths][hundredths]
#define SLOT_PERIOD  3

// Title: shown during MODE_ATTRACT, drawn as ONE continuous canvas spanning
// all 8 sprite channels tiled edge-to-edge. Each channel is a hardware-
// guaranteed 16-native-dot-wide, non-overlapping slice of that canvas, so
// letters can't overlap each other or go missing regardless of any
// lores-vs-native unit uncertainty in the maths below (that was the bug:
// per-letter HSTART spacing computed in the wrong unit). Only the canvas's
// overall on-screen placement (TITLE_X) is a best-effort centring - nudge it
// if the whole banner isn't quite centred, that's independent of overlap.
#define TITLE_LETTERS        7  // "HEXAGON"
#define TITLE_CANVAS_BITS    (SPRITE_CHANNELS * 16) // 128 native dots across all 8 sprites
#define TITLE_GLYPH_NATIVE_W (HUD_GLYPH_W * 2)       // each glyph column doubled, same as the HUD font
#define TITLE_GAP_NATIVE     6                        // gap between letters, in native dots
#define TITLE_MARGIN_NATIVE  2                        // white margin each side of the whole banner
#define TITLE_CELL_NATIVE    (TITLE_GLYPH_NATIVE_W + TITLE_GAP_NATIVE)
#define TITLE_BANNER_W       (TITLE_MARGIN_NATIVE * 2 + TITLE_LETTERS * TITLE_GLYPH_NATIVE_W \
                               + (TITLE_LETTERS - 1) * TITLE_GAP_NATIVE) // native dots, fits within TITLE_CANVAS_BITS
#define TITLE_BANNER_X       ((TITLE_CANVAS_BITS - TITLE_BANNER_W) / 2)  // centred within the canvas
// Measured correction: the best-effort screen-centring guess landed ~2.5
// characters too far right. TITLE_X feeds directly into the same native-dot
// HSTART space as the per-channel +16 spacing, so "2.5 characters" converts
// exactly to 2.5*TITLE_CELL_NATIVE here with no lores/native guessing needed.
#define TITLE_X_ADJUST (35) // 2.5 * TITLE_CELL_NATIVE(14), measured
#define TITLE_X       (((SCREEN_WIDTH - TITLE_CANVAS_BITS / 2) / 2) - TITLE_X_ADJUST)
#define TITLE_Y       24 // screen px from the top - clear of the hub and the corner HUD
#define TITLE_HOLD_TICKS  (FRAME_RATE * 3 / 10) // ~0.3s hold once READY starts
#define TITLE_FLASH_TICKS 3                     // one-tick-ish white flash as it cuts away

// Raw 4-bit rows, MSB = leftmost column. Blocky but legible at this size.
static const UBYTE font[GLYPH_COUNT][HUD_GLYPH_H] = {
    /* 0 */ { 0xF, 0x9, 0x9, 0x9, 0x9, 0xF },
    /* 1 */ { 0x2, 0x6, 0x2, 0x2, 0x2, 0x7 },
    /* 2 */ { 0xF, 0x1, 0x1, 0xF, 0x8, 0xF },
    /* 3 */ { 0xF, 0x1, 0x7, 0x1, 0x1, 0xF },
    /* 4 */ { 0x9, 0x9, 0x9, 0xF, 0x1, 0x1 },
    /* 5 */ { 0xF, 0x8, 0xF, 0x1, 0x1, 0xF },
    /* 6 */ { 0xF, 0x8, 0xF, 0x9, 0x9, 0xF },
    /* 7 */ { 0xF, 0x1, 0x2, 0x4, 0x4, 0x4 },
    /* 8 */ { 0xF, 0x9, 0xF, 0x9, 0x9, 0xF },
    /* 9 */ { 0xF, 0x9, 0xF, 0x1, 0x1, 0xF },
    /* : */ { 0x0, 0x2, 0x0, 0x2, 0x0, 0x0 },
    /* . */ { 0x0, 0x0, 0x0, 0x0, 0x0, 0x2 },
};

// "HEXAGON", one row set per letter, same 4-bit/row encoding as the digits.
static const UBYTE title_font[TITLE_LETTERS][HUD_GLYPH_H] = {
    /* H */ { 0x9, 0x9, 0x9, 0xF, 0x9, 0x9 },
    /* E */ { 0xF, 0x8, 0xE, 0x8, 0x8, 0xF },
    /* X */ { 0x9, 0x9, 0x6, 0x6, 0x9, 0x9 },
    /* A */ { 0x6, 0x9, 0x9, 0xF, 0x9, 0x9 },
    /* G */ { 0x7, 0x8, 0x8, 0xB, 0x9, 0x7 },
    /* O */ { 0x6, 0x9, 0x9, 0x9, 0x9, 0x6 },
    /* N */ { 0x9, 0xD, 0xD, 0xB, 0xB, 0x9 },
};

// One pre-built, STATIC sprite descriptor (pos, ctl, data rows, terminator)
// per (slot, glyph) and per title letter: position and content are both
// fixed at hud_init() time and never change. "Showing" something just means
// which buffer's ADDRESS the copper hands to SPRxPT that frame (see
// hud_emit_copper / hud.h) - the CPU never writes SPRxPT or any sprite data.
static UWORD* glyph_buf[HUD_SLOTS][GLYPH_COUNT];
static UBYTE  cur_glyph[HUD_SLOTS]; // this frame's HUD choice per slot, from hud_tick()
static UWORD* title_buf[SPRITE_CHANNELS]; // one canvas slice per channel

// A degenerate (0,0) pos/ctl descriptor: 0-height, so the DMA channel goes
// inert for the rest of that frame. Parked on every sprite channel that
// isn't showing something this frame, and reloaded every frame exactly like
// the active ones. SPRxPT is a working pointer, not a "redraw this every
// vblank" register: Agnus auto-increments it as it fetches pos/ctl/data, so
// a channel that isn't rewritten doesn't stay put, it walks forward through
// chip RAM one frame at a time and eventually fetches (and displays)
// whatever it wanders into.
static const UWORD blank_sprite[2] __attribute__((section(".MEMF_CHIP"))) = { 0, 0 };

// Double each bit of a `width`-bit row so an authored "pixel" is 2 sprite dots
// wide, matching the display's lores pixel size (sprites are always hires-pitch).
static UWORD double_bits(UWORD n, WORD width) {
    UWORD r = 0;
    for (WORD b = width - 1; b >= 0; b--) {
        UWORD bit = (n >> b) & 1u;
        r = (UWORD)((r << 2) | (bit << 1) | bit);
    }
    return r; // 2*width bits, in the low bits
}

// Each sprite pair shares a 3-colour bank: value 1/2/3 (value 0 is always
// transparent). Pairs (0,1)/(2,3)/(4,5)/(6,7) sit at colour 17/21/25/29.
// The digit HUD only ever used channels 0-5 (3 banks); the title canvas
// uses all 8 (channel 6/7 = the 4th pair), which is what was missing here -
// bank 29 was left at TakeSystem's zeroed default, so both "black" and
// "white" for that pair resolved to 0x000: a solid black tile.
static void set_hud_colours(void) {
    static const UBYTE bank_base[4] = { 17, 21, 25, 29 };
    for (WORD i = 0; i < 4; i++) {
        custom->color[bank_base[i] + 0] = 0x000; // value 1: glyph foreground - black
        custom->color[bank_base[i] + 1] = 0xfff; // value 2: glyph background - white
    }
}

static void pos_ctl(UWORD hstart, UWORD vstart, UWORD* out_pos, UWORD* out_ctl) {
    UWORD vstop = vstart + HUD_CELL_H;
    *out_pos = (UWORD)(((vstart & 0xff) << 8) | ((hstart >> 1) & 0xff));
    *out_ctl = (UWORD)(((vstop & 0xff) << 8)
             | (((vstart >> 8) & 1) << 2)
             | (((vstop  >> 8) & 1) << 1)
             | (hstart & 1));
}

// Builds one static sprite descriptor: pos/ctl + a bordered rendering of
// `rows` (HUD_GLYPH_H entries, same encoding as font[]/title_font[]) + terminator.
static UWORD* build_glyph(UWORD pos, UWORD ctl, const UBYTE* rows) {
    UWORD* buf = (UWORD*)AllocMem(
        (2 + HUD_CELL_H * 2 + 2) * sizeof(UWORD), MEMF_CHIP | MEMF_CLEAR);
    if (!buf) return 0;

    UWORD* p = buf;
    *p++ = pos;
    *p++ = ctl;
    // Row 0 and the last row are pure border; the font rows sit indented by
    // HUD_BORDER columns in between, giving a 1px white margin on all four
    // sides of the glyph.
    for (WORD row = 0; row < HUD_CELL_H; row++) {
        UBYTE fontrow = 0;
        if (row >= HUD_BORDER && row < HUD_BORDER + HUD_GLYPH_H)
            fontrow = rows[row - HUD_BORDER];
        UWORD on  = (UWORD)fontrow << HUD_BORDER; // glyph pixels: value 1 (black)
        UWORD off = (UWORD)(~on) & HUD_CELL_MASK;  // everything else: value 2 (white)
        *p++ = (UWORD)(double_bits(on,  HUD_CELL_W) << HUD_SHIFT); // bitplane 0 (value bit 0)
        *p++ = (UWORD)(double_bits(off, HUD_CELL_W) << HUD_SHIFT); // bitplane 1 (value bit 1)
    }
    *p++ = 0;
    *p++ = 0; // terminator
    return buf;
}

// Like build_glyph, but leaves the data rows blank (transparent) for
// canvas_set() to paint in afterwards, since a title canvas slice's content
// depends on where it falls relative to the whole 8-sprite banner, not on
// itself in isolation.
static UWORD* alloc_canvas_slice(UWORD pos, UWORD ctl) {
    UWORD* buf = (UWORD*)AllocMem(
        (2 + HUD_CELL_H * 2 + 2) * sizeof(UWORD), MEMF_CHIP | MEMF_CLEAR);
    if (!buf) return 0;
    buf[0] = pos;
    buf[1] = ctl;
    return buf;
}

// Sets native column `nx` (0..TITLE_CANVAS_BITS-1) of canvas row `row` to
// black (bitplane0/value1) or white (bitplane1/value2), in whichever
// channel's slice that column falls in. Exclusive: painting black over a
// spot that was white (banner background drawn first, glyph strokes after)
// clears the white bit too, rather than leaving both set (value 3, which
// was never assigned a colour - it happened to render black anyway only
// because color[bank+2] defaults to 0x000).
static void canvas_set(WORD row, WORD nx, UBYTE black) {
    if (nx < 0 || nx >= TITLE_CANVAS_BITS) return;
    WORD ch = nx >> 4;         // /16 - each slice is exactly one sprite wide
    WORD bit = 15 - (nx & 15); // bit position within that channel's row word
    UWORD* buf = title_buf[ch];
    if (!buf) return;
    UWORD* row_words = buf + 2 + row * 2; // skip pos, ctl
    UWORD mask = (UWORD)(1u << bit);
    if (black) { row_words[0] |= mask; row_words[1] &= (UWORD)~mask; }
    else       { row_words[1] |= mask; row_words[0] &= (UWORD)~mask; }
}

void hud_init(void) {
    set_hud_colours();
    // No sprpt[] writes here: hud_emit_copper() reloads every channel, every
    // frame (see blank_sprite above for why "set once" doesn't work).

    // Slots are a full sprite-width (16 native dots) apart, same reasoning as
    // the title canvas below: that's a hardware-guaranteed non-overlapping
    // spacing regardless of how HSTART's native unit maps to screen pixels,
    // unlike the old HUD_SPACING-based offset (too tight to read - the same
    // per-character-HSTART unit uncertainty that broke the title's spacing).
    for (WORD slot = 0; slot < HUD_SLOTS; slot++) {
        UWORD hstart = DISPLAY_HW_X + HUD_X + slot * 16;
        UWORD pos, ctl;
        pos_ctl(hstart, DISPLAY_HW_Y + HUD_Y, &pos, &ctl);
        for (WORD g = 0; g < GLYPH_COUNT; g++)
            glyph_buf[slot][g] = build_glyph(pos, ctl, font[g]);
    }

    // Title canvas: 8 sprites tiled edge-to-edge (16 native dots apart -
    // exactly one sprite width, so adjacent slices can't overlap or gap),
    // forming one 128-dot strip. The banner (white background + black
    // letters) is then drawn straight across it in native-dot coordinates.
    {
        UWORD base_hstart = DISPLAY_HW_X + TITLE_X;
        for (WORD ch = 0; ch < SPRITE_CHANNELS; ch++) {
            UWORD pos, ctl;
            pos_ctl(base_hstart + ch * 16, DISPLAY_HW_Y + TITLE_Y, &pos, &ctl);
            title_buf[ch] = alloc_canvas_slice(pos, ctl);
        }

        for (WORD row = 0; row < HUD_CELL_H; row++) {
            for (WORD nx = TITLE_BANNER_X; nx < TITLE_BANNER_X + TITLE_BANNER_W; nx++)
                canvas_set(row, nx, 0); // white banner background

            if (row < HUD_BORDER || row >= HUD_BORDER + HUD_GLYPH_H)
                continue; // pure border row - background only, no glyph pixels

            UBYTE frow = row - HUD_BORDER;
            for (WORD letter = 0; letter < TITLE_LETTERS; letter++) {
                WORD cell_x = TITLE_BANNER_X + TITLE_MARGIN_NATIVE + letter * TITLE_CELL_NATIVE;
                UBYTE bits = title_font[letter][frow];
                for (WORD c = 0; c < HUD_GLYPH_W; c++) {
                    if (!(bits & (1 << (HUD_GLYPH_W - 1 - c))))
                        continue;
                    canvas_set(row, cell_x + c * 2,     1); // doubled, matches the HUD font's sizing
                    canvas_set(row, cell_x + c * 2 + 1, 1);
                }
            }
        }
    }
}

// Title shows over the whole of ATTRACT, then holds briefly into READY
// before hud_flash_now() cuts it away to the timer HUD.
static UBYTE title_active(void) {
    GameMode m = game_mode();
    if (m == MODE_ATTRACT) return 1;
    if (m == MODE_READY && game_mode_timer() < TITLE_HOLD_TICKS) return 1;
    return 0;
}

UBYTE hud_flash_now(void) {
    UWORD t = game_mode_timer();
    return (UBYTE)(game_mode() == MODE_READY
        && t >= TITLE_HOLD_TICKS
        && t < TITLE_HOLD_TICKS + TITLE_FLASH_TICKS);
}

void hud_tick(void) {
    GameMode m = game_mode();
    UWORD secs, frames;

    // Current run while it's live, otherwise the best time on record.
    if (m == MODE_PLAYING || m == MODE_DEAD) {
        secs = gamestate.time_seconds;
        frames = gamestate.time_subsecond_frames;
    } else {
        secs = gamestate.record_seconds;
        frames = gamestate.record_subsecond_frames;
    }
    if (secs > 999) secs = 999;

    UWORD prod = (UWORD)muluw(frames, 100); // frames < FRAME_RATE <= 60, fits UWORD
    UWORD cs = prod / FRAME_RATE;
    if (cs > 99) cs = 99;

    cur_glyph[0] = (UBYTE)(secs / 100);
    cur_glyph[1] = (UBYTE)((secs / 10) % 10);
    cur_glyph[2] = (UBYTE)(secs % 10);
    cur_glyph[SLOT_PERIOD] = GLYPH_PERIOD;
    cur_glyph[4] = (UBYTE)(cs / 10);
    cur_glyph[5] = (UBYTE)(cs % 10);
}

USHORT* hud_emit_copper(USHORT* copPtr) {
    // Reloads all 8 sprite channels, every frame - see blank_sprite above.
    // Always emits exactly SPRITE_CHANNELS*4 words: the main loop rewrites
    // this same-length tail of the copper list every frame, so a short
    // write here (e.g. skipping a channel whose AllocMem somehow failed)
    // would desync it from what the rest of the list expects to follow -
    // fall back to the inert blank_sprite rather than ever skipping one.
    UBYTE showing_title = title_active();
    for (WORD ch = 0; ch < SPRITE_CHANNELS; ch++) {
        UWORD* buf = (UWORD*)blank_sprite;
        if (showing_title) {
            if (title_buf[ch]) buf = title_buf[ch]; // all 8 channels carry a canvas slice
        } else if (ch < HUD_SLOTS) {
            UWORD* g = glyph_buf[ch][cur_glyph[ch]];
            if (g) buf = g;
        }
        UWORD offset = (UWORD)(offsetof(struct Custom, sprpt) + ch * sizeof(APTR));
        copPtr = copWritePtr(copPtr, offset, buf);
    }
    return copPtr;
}
