#include "hud.h"
#include "config.h"
#include "system.h"  // custom, muluw, AllocMem
#include "coplist.h" // copWritePtr
#include "game.h"
#include "render.h"
#include "blitter.h"
#include "menu_font16.h"
#include "menu_carousel.h"
#include "credit_qr.h"
#pragma GCC optimize ("Os")

static UBYTE *front_planes;
static ULONG front_keys[3],front_scores[3];
static int front_positions[3];
static unsigned front_strip_page;

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

// Title/banners: shown during MODE_ATTRACT and briefly around mode
// transitions, drawn as ONE continuous canvas spanning all 8 sprite channels
// tiled edge-to-edge. Each channel is a hardware-guaranteed 16-native-dot-wide,
// non-overlapping slice of that canvas, so letters can't overlap each other
// or go missing regardless of any lores-vs-native unit uncertainty in the
// maths below (that was the bug: per-letter HSTART spacing computed in the
// wrong unit). Only the canvas's overall on-screen placement (TITLE_X) is a
// best-effort centring - nudge it if the whole banner isn't quite centred,
// that's independent of overlap. Banner width/centring is computed per
// string (see paint_banner) so different messages ("HEXAGON", "GAME OVER")
// can share this machinery at their own natural widths.
#define TITLE_CANVAS_BITS    (SPRITE_CHANNELS * 16) // 128 native dots across all 8 sprites
#define TITLE_GLYPH_NATIVE_W (HUD_GLYPH_W * 2)       // each glyph column doubled, same as the HUD font
#define TITLE_GAP_NATIVE     6                        // gap between letters, in native dots
#define TITLE_MARGIN_NATIVE  2                        // white margin each side of the whole banner
#define TITLE_CELL_NATIVE    (TITLE_GLYPH_NATIVE_W + TITLE_GAP_NATIVE)
// Measured correction: the best-effort screen-centring guess landed ~2.5
// characters too far right. TITLE_X feeds directly into the same native-dot
// HSTART space as the per-channel +16 spacing, so "2.5 characters" converts
// exactly to 2.5*TITLE_CELL_NATIVE here with no lores/native guessing needed.
#define TITLE_X_ADJUST (35) // 2.5 * TITLE_CELL_NATIVE(14), measured
#define TITLE_X       (((SCREEN_WIDTH - TITLE_CANVAS_BITS / 2) / 2) - TITLE_X_ADJUST)
#define TITLE_Y       24 // screen px from the top - clear of the hub and the corner HUD

#if PLAYER_SPRITE_RELOAD_Y < TITLE_Y + HUD_CELL_H + 2
#error "Player sprite reload must follow the banner's DMA terminator"
#endif

#define GAMEOVER_HOLD_TICKS  (FRAME_RATE * 16 / 10) // ~1.6s "GAME OVER" banner (doubled per user request)
#define GAMEOVER_FLASH_TICKS 3                      // white flash as it cuts to the result readout
#define RECORD_FLASH_PERIOD  16                     // ticks per on/off half of the "new record" blink

// Raw 4-bit rows, MSB = leftmost column. Blocky but legible at this size.
static const UBYTE font[GLYPH_COUNT][HUD_GLYPH_H] = {
    /* 0 */ {
        0b1111,
        0b1001,
        0b1001,
        0b1001,
        0b1001,
        0b1111
    },
    /* 1 */ {
        0b0010,
        0b0110,
        0b0010,
        0b0010,
        0b0010,
        0b0111
    },
    /* 2 */ {
        0b1111,
        0b0001,
        0b0001,
        0b1111,
        0b1000,
        0b1111
    },
    /* 3 */ {
        0b1111,
        0b0001,
        0b0111,
        0b0001,
        0b0001,
        0b1111
    },
    /* 4 */ {
        0b1001,
        0b1001,
        0b1001,
        0b1111,
        0b0001,
        0b0001
    },
    /* 5 */ {
        0b1111,
        0b1000,
        0b1111,
        0b0001,
        0b0001,
        0b1111
    },
    /* 6 */ {
        0b1111,
        0b1000,
        0b1111,
        0b1001,
        0b1001,
        0b1111
    },
    /* 7 */ {
        0b1111,
        0b0001,
        0b0010,
        0b0100,
        0b0100,
        0b0100
    },
    /* 8 */ {
        0b1111,
        0b1001,
        0b1111,
        0b1001,
        0b1001,
        0b1111
    },
    /* 9 */ {
        0b1111,
        0b1001,
        0b1111,
        0b0001,
        0b0001,
        0b1111
    },
    /* : */ {
        0b0000,
        0b0010,
        0b0000,
        0b0010,
        0b0000,
        0b0000
    },
    /* . */ {
        0b0000,
        0b0000,
        0b0000,
        0b0000,
        0b0000,
        0b0010
    },
};

// Letter glyphs for the banner canvas, same 4-bit/row encoding as the digit
// font. Only the letters actually needed by title_str_*[] below are
// authored (not a full alphabet) - add more here as more banners want them.
enum {
    TF_H, TF_E, TF_X, TF_A, TF_G, TF_O, TF_N, TF_M, TF_V, TF_R, TF_SPACE, TF_S, TF_T, TF_Y, TF_P, TF_L, TF_C, TF_K, TF_D, TF_W, TF_I, TF_U,
    TITLE_GLYPH_COUNT
};
static const UBYTE title_font[TITLE_GLYPH_COUNT][HUD_GLYPH_H] = {
    /* H */ {
        0b1001,
        0b1001,
        0b1001,
        0b1111,
        0b1001,
        0b1001
    },
    /* E */ {
        0b1111,
        0b0000,
        0b1110,
        0b1000,
        0b1000,
        0b1111
    },
    /* X */ {
        0b1001,
        0b1001,
        0b0110,
        0b0110,
        0b1001,
        0b1001
    },
    /* A */ {
        0b0110,
        0b0001,
        0b1001,
        0b1111,
        0b1001,
        0b1001
    },
    /* G */ {
        0b0111,
        0b0000,
        0b1000,
        0b1011,
        0b1001,
        0b0111
    },
    /* O */ {
        0b0110,
        0b0001,
        0b1001,
        0b1001,
        0b1001,
        0b0110
    },
    /* N */ {
        0b1001,
        0b1101,
        0b1101,
        0b1011,
        0b1011,
        0b1001
    },
    /* M */ {
        0b1111,
        0b0000,
        0b1111,
        0b1001,
        0b1001,
        0b1001
    },
    /* V */ {
        0b1001,
        0b1001,
        0b1001,
        0b1001,
        0b0110,
        0b0110
    },
    /* R */ {
        0b1110,
        0b0001,
        0b1110,
        0b1010,
        0b1001,
        0b1001
    },
    /* (space, all blank) */ {
        0b0000,
        0b0000,
        0b0000,
        0b0000,
        0b0000,
        0b0000
    },
    /* S */ {
        0b0111,
        0b0000,
        0b0110,
        0b0001,
        0b0001,
        0b1110
    },
    /* T */ {
        0b1111,
        0b0010,
        0b0010,
        0b0010,
        0b0010,
        0b0010
    },
    /* Y */ {
        0b1001,
        0b1001,
        0b0110,
        0b0010,
        0b0010,
        0b0010
    },
    /* P */ {
        0b1110,
        0b0001,
        0b1001,
        0b1110,
        0b1000,
        0b1000
    },
    /* L */ {
        0b1000,
        0b1000,
        0b1000,
        0b1000,
        0b1000,
        0b1111
    },
    /* C */ {
        0b0111,
        0b0000,
        0b1000,
        0b1000,
        0b1000,
        0b0111
    },
    /* K */ {
        0b1001,
        0b1010,
        0b1100,
        0b1010,
        0b1001,
        0b1001
    },
    /* D */ {
        0b1110,
        0b0001,
        0b1001,
        0b1001,
        0b1001,
        0b1110
    },
    /* W */ {9,9,9,15,15,9},
    /* I */ {15,6,6,6,6,15},
    /* U */ {9,9,9,9,9,6},
};

static const UBYTE title_saving[]={TF_S,TF_A,TF_V,TF_E};
static const UBYTE title_write_protected[]={TF_W,TF_R,TF_I,TF_T,TF_E,TF_SPACE,TF_P,TF_R,TF_O,TF_T,TF_E,TF_C,TF_T,TF_E,TF_D};
static const UBYTE title_save_error[]={TF_S,TF_A,TF_V,TF_E,TF_SPACE,TF_E,TF_R,TF_R,TF_O,TF_R};
static const UBYTE title_loading[]={TF_L,TF_O,TF_A,TF_D,TF_SPACE,TF_T,TF_R,TF_A,TF_C,TF_K};
static const UBYTE title_load_error[]={TF_L,TF_O,TF_A,TF_D,TF_SPACE,TF_E,TF_R,TF_R,TF_O,TF_R};
static const UBYTE title_str_gameover[] = {
    TF_G,
    TF_A,
    TF_M,
    TF_E,
    TF_SPACE,
    TF_O,
    TF_V,
    TF_E,
    TF_R
};

// One pre-built, STATIC sprite descriptor (pos, ctl, data rows, terminator)
// per (slot, glyph) and per title letter: position and content are both
// fixed at hud_init() time and never change. "Showing" something just means
// which buffer's ADDRESS the copper hands to SPRxPT that frame (see
// hud_emit_copper / hud.h) - the CPU never writes SPRxPT or any sprite data.
static UWORD* glyph_buf[HUD_SLOTS][GLYPH_COUNT];
static UBYTE  cur_glyph[HUD_SLOTS]; // this frame's HUD choice per slot, from hud_tick()
static UBYTE centisecond_digits[FRAME_RATE]; // packed decimal, indexed by tick
static UWORD glyph_seconds=0xffff;
static UWORD* load_error_buf[SPRITE_CHANNELS];
#if TRACKLOADER
static UWORD* loading_buf[SPRITE_CHANNELS],*saving_buf[SPRITE_CHANNELS],*save_error_buf[SPRITE_CHANNELS];
static UWORD *loading_sprite_pointers;
static UWORD *write_protected_buf[SPRITE_CHANNELS];
static UBYTE save_failed;
static UWORD save_failed_at;
void hud_save_failed(UBYTE failed) {
    save_failed=failed;
    save_failed_at=game_mode_timer();
}
/* The native executable, including BSS, resides entirely in Chip RAM. */
static UWORD loading_copper[256];
static volatile UWORD *busy_waits[6];
static UWORD busy_frame;
static APTR busy_previous_irq;
/* Leave 16 horizontal counts between colour transitions: each transition
 * writes both playfield colours before the Copper can fetch its next WAIT.
 * In particular, do not let the moving segment crowd the fixed right edge. */
enum { BUSY_LEFT=0x51, BUSY_START=0x61, BUSY_TRAVEL=32,
       BUSY_WIDTH=0x10, BUSY_RIGHT=0xc1 };
/* Only copper WAIT coordinates change. No game, audio, disk or blitter state
 * is touched, and frameCounter deliberately does not advance during I/O. */
__attribute__((interrupt)) static void loading_interrupt(void) {
    custom->intreq=INTF_VERTB;custom->intreq=INTF_VERTB;
    UWORD x=(busy_frame++ >> 1)%(2*BUSY_TRAVEL);
    if(x>BUSY_TRAVEL)x=2*BUSY_TRAVEL-x;
    x=BUSY_START+2*x;
    for(unsigned row=0;row<6;++row) {
        volatile UWORD *p=busy_waits[row];
        p[0]=(p[0]&0xff00)|x;
        p[6]=(p[6]&0xff00)|(x+BUSY_WIDTH);
    }
}
void hud_loading_begin(void) {
    UWORD sr;
    __asm volatile("move.w %%sr,%0\n move.w #0x2700,%%sr":"=d"(sr)::"memory","cc");
    busy_frame=0;busy_previous_irq=GetInterruptHandler();
    SetInterruptHandler((APTR)loading_interrupt);
    __asm volatile("move.w %0,%%sr"::"d"(sr):"memory","cc");
}
void hud_loading_end(void) {
    UWORD sr;
    __asm volatile("move.w %%sr,%0\n move.w #0x2700,%%sr":"=d"(sr)::"memory","cc");
    SetInterruptHandler(busy_previous_irq);
    __asm volatile("move.w %0,%%sr"::"d"(sr):"memory","cc");
}

UWORD* hud_loading_copper(void *plane,UBYTE saving) {
    UWORD *cp=loading_sprite_pointers;
    for (unsigned ch=0;ch<SPRITE_CHANNELS;ch++)
        cp=copWritePtr(cp,offsetof(struct Custom,sprpt)+ch*sizeof(APTR),saving?saving_buf[ch]:loading_buf[ch]);
    copWritePtr(loading_copper,offsetof(struct Custom,bplpt[0]),plane);
    return loading_copper;
}
#endif
static UWORD* gameover_buf[SPRITE_CHANNELS]; // "GAME OVER" canvas, same layout

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
/* Size-optimise startup bitmap construction; frame rendering keeps O2. */
#pragma GCC push_options
#pragma GCC optimize ("Os")
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
// The digit HUD only ever used channels 0-5 (3 banks, bank_base[0..2]); the
// title/banner canvases use all 8 (channel 6/7 = the 4th pair, bank_base[3]) -
// this is why the array has 4 entries even though the digit HUD alone would
// only need 3 (missing bank 29 here once caused a solid-black tile: it was
// left at TakeSystem's zeroed default, so both "black" and "white" for that
// pair resolved to 0x000).
static const UBYTE bank_base[4] = { 17, 21, 25, 29 };

static void set_hud_colours(void) {
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
    UWORD* buf = (UWORD*)GameAllocChip(
        (2 + HUD_CELL_H * 2 + 2) * sizeof(UWORD));
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
// paint_banner()/canvas_set_into() to paint in afterwards, since a canvas
// slice's content depends on where it falls relative to the whole 8-sprite
// banner, not on itself in isolation.
static UWORD* alloc_canvas_slice(UWORD pos, UWORD ctl) {
    UWORD* buf = (UWORD*)GameAllocChip(
        (2 + HUD_CELL_H * 2 + 2) * sizeof(UWORD));
    if (!buf) return 0;
    buf[0] = pos;
    buf[1] = ctl;
    return buf;
}

// Sets native column `nx` (0..TITLE_CANVAS_BITS-1) of canvas row `row` to
// black (bitplane0/value1) or white (bitplane1/value2), in whichever
// channel's slice of `slots` that column falls in. Exclusive: painting black
// over a spot that was white (banner background drawn first, glyph strokes
// after) clears the white bit too, rather than leaving both set (value 3,
// which was never assigned a colour - it happened to render black anyway
// only because color[bank+2] defaults to 0x000).
static void canvas_set_into(UWORD* const* slots, WORD row, WORD nx, UBYTE black) {
    if (nx < 0 || nx >= TITLE_CANVAS_BITS) return;
    WORD ch = nx >> 4;         // /16 - each slice is exactly one sprite wide
    WORD bit = 15 - (nx & 15); // bit position within that channel's row word
    UWORD* buf = slots[ch];
    if (!buf) return;
    UWORD* row_words = buf + 2 + row * 2; // skip pos, ctl
    UWORD mask = (UWORD)(1u << bit);
    if (black) { row_words[0] |= mask; row_words[1] &= (UWORD)~mask; }
    else       { row_words[1] |= mask; row_words[0] &= (UWORD)~mask; }
}

// Paints a white background banner + black glyph strokes for `str` (an
// array of title_font indices, `len` long) into `slots`, centred within the
// 128-dot canvas at that string's own natural width. Shared by every banner
// message (title, results) - see title_str_hexagon/title_str_gameover.
static void paint_banner(UWORD* const* slots, const UBYTE* str, WORD len) {
    WORD gap = TITLE_GAP_NATIVE;
    /* HYPER HEXAGONEST fits all 128 sprite dots with a half-width space. */
    WORD space_width=len>15 ? TITLE_GLYPH_NATIVE_W/2:TITLE_GLYPH_NATIVE_W;
    WORD text_width=len*TITLE_GLYPH_NATIVE_W;
    for (WORD i=0;i<len;++i)
        if (str[i]==TF_SPACE) text_width-=TITLE_GLYPH_NATIVE_W-space_width;
    if (len > 1) {
        WORD available = (TITLE_CANVAS_BITS - 2 * TITLE_MARGIN_NATIVE
                          - text_width) / (len - 1);
        if (gap > available) gap = available;
    }
    WORD banner_w = TITLE_MARGIN_NATIVE * 2 + text_width
                     + (len - 1) * gap;
    WORD banner_x = (TITLE_CANVAS_BITS - banner_w) / 2;

    for (WORD row = 0; row < HUD_CELL_H; row++) {
        for (WORD nx = banner_x; nx < banner_x + banner_w; nx++)
            canvas_set_into(slots, row, nx, 0); // white banner background

        if (row < HUD_BORDER || row >= HUD_BORDER + HUD_GLYPH_H)
            continue; // pure border row - background only, no glyph pixels

        UBYTE frow = row - HUD_BORDER;
        WORD cell_x=banner_x+TITLE_MARGIN_NATIVE;
        for (WORD letter = 0; letter < len; letter++) {
            UBYTE bits = title_font[str[letter]][frow];
            for (WORD c = 0; c < HUD_GLYPH_W; c++) {
                if (!(bits & (1 << (HUD_GLYPH_W - 1 - c))))
                    continue;
                canvas_set_into(slots, row, cell_x + c * 2,     1); // doubled, matches the HUD font's sizing
                canvas_set_into(slots, row, cell_x + c * 2 + 1, 1);
            }
            cell_x+=(str[letter]==TF_SPACE ? space_width:TITLE_GLYPH_NATIVE_W)+gap;
        }
    }
}

static void free_sprite(UWORD **buf) {
    if (*buf) GameFreeChip(*buf, (2 + HUD_CELL_H * 2 + 2) * sizeof(UWORD));
    *buf = 0;
}

__attribute__((optimize("Os"))) void hud_free(void) {
    if(front_planes) {GameFreeChip(front_planes,3*BITPLANE_SIZE+MENU_STRIP_SIZE);front_planes=0;}
    for (WORD slot = 0; slot < HUD_SLOTS; ++slot)
        for (WORD g = 0; g < GLYPH_COUNT; ++g)
            free_sprite(&glyph_buf[slot][g]);
    for (WORD ch = 0; ch < SPRITE_CHANNELS; ++ch) {
        free_sprite(&load_error_buf[ch]);
#if TRACKLOADER
        free_sprite(&loading_buf[ch]);free_sprite(&saving_buf[ch]);free_sprite(&save_error_buf[ch]);free_sprite(&write_protected_buf[ch]);
#endif
        free_sprite(&gameover_buf[ch]);
    }
}

__attribute__((optimize("Os"))) void hud_init(void) {
    front_planes=GameAllocChip(3*BITPLANE_SIZE+MENU_STRIP_SIZE);
    front_strip_page=0;
    if(front_planes)menu_strip_init(front_planes+3*BITPLANE_SIZE);
    for(unsigned i=0;i<3;++i)front_keys[i]=~0u;
    glyph_seconds=0xffff;
    for (UWORD tick=0;tick<FRAME_RATE;++tick) {
        UWORD cs=tick*100/FRAME_RATE;
        centisecond_digits[tick]=(UBYTE)(((cs/10)<<4)|(cs%10));
    }
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
        /* The decimal slot never shows digits; other slots never show
         * punctuation. Keep only sprites that hud_tick can select. */
        if(slot==SLOT_PERIOD)
            glyph_buf[slot][GLYPH_PERIOD]=build_glyph(pos,ctl,font[GLYPH_PERIOD]);
        else for(WORD g=0;g<10;++g)
            glyph_buf[slot][g]=build_glyph(pos,ctl,font[g]);
    }

    // Banner canvases: 8 sprites tiled edge-to-edge (16 native dots apart -
    // exactly one sprite width, so adjacent slices can't overlap or gap),
    // forming one 128-dot strip. Both banners share the same on-screen
    // position (they're never shown at the same time - see banner_active()).
    {
        UWORD base_hstart = DISPLAY_HW_X + TITLE_X;
        for (WORD ch = 0; ch < SPRITE_CHANNELS; ch++) {
            UWORD pos, ctl;
            pos_ctl(base_hstart + ch * 16, DISPLAY_HW_Y + TITLE_Y, &pos, &ctl);
            load_error_buf[ch]=alloc_canvas_slice(pos,ctl);
#if TRACKLOADER
            loading_buf[ch]=alloc_canvas_slice(pos,ctl);
            saving_buf[ch]=alloc_canvas_slice(pos,ctl);save_error_buf[ch]=alloc_canvas_slice(pos,ctl);
            write_protected_buf[ch]=alloc_canvas_slice(pos,ctl);
#endif
            gameover_buf[ch] = alloc_canvas_slice(pos, ctl);
        }
        paint_banner(load_error_buf,title_load_error,sizeof(title_load_error));
#if TRACKLOADER
        paint_banner(loading_buf,title_loading,sizeof(title_loading));
        paint_banner(saving_buf,title_saving,sizeof(title_saving));
        paint_banner(save_error_buf,title_save_error,sizeof(title_save_error));
        paint_banner(write_protected_buf,title_write_protected,sizeof(title_write_protected));
        UWORD *cp=loading_copper+4; /* plane pointer filled before activation */
        /* Activity uses the background colour and banner sprites only.
         * Fetching the old gameplay plane exposes its pixels between the
         * sequential COLOR00/COLOR01 writes at every bar transition. */
        cp=copWrite(cp,offsetof(struct Custom,bplcon0),BPLCON0F_COLOR);
        cp=copWrite(cp,offsetof(struct Custom,color[1]),0);
        cp=copWrite(cp,offsetof(struct Custom,color[0]),0);
        for (WORD bank=0;bank<4;++bank) {
            cp=copWrite(cp,offsetof(struct Custom,color[17])+bank*8,0);
            cp=copWrite(cp,offsetof(struct Custom,color[18])+bank*8,0xfff);
        }
        loading_sprite_pointers=cp;
        for (WORD ch=0;ch<SPRITE_CHANNELS;++ch)
            cp=copWritePtr(cp,offsetof(struct Custom,sprpt)+ch*sizeof(APTR),
                loading_buf[ch] ? loading_buf[ch] : (UWORD*)blank_sprite);
        /* Six raster lines: grey rail with a bouncing white segment. WAIT
         * words are patched only at VBlank, before these display lines. */
        for(unsigned row=0;row<6;++row) {
            UWORD y=(DISPLAY_HW_Y+TITLE_Y+24+row)<<8;
            *cp++=y|BUSY_LEFT;*cp++=0xfffe;
            cp=copWrite(cp,offsetof(struct Custom,color[0]),0x333);
            cp=copWrite(cp,offsetof(struct Custom,color[1]),0x333);
            busy_waits[row]=cp;
            *cp++=y|BUSY_START;*cp++=0xfffe;
            cp=copWrite(cp,offsetof(struct Custom,color[0]),0xfff);
            cp=copWrite(cp,offsetof(struct Custom,color[1]),0xfff);
            *cp++=y|(BUSY_START+BUSY_WIDTH);*cp++=0xfffe;
            cp=copWrite(cp,offsetof(struct Custom,color[0]),0x333);
            cp=copWrite(cp,offsetof(struct Custom,color[1]),0x333);
            *cp++=y|BUSY_RIGHT;*cp++=0xfffe;
            cp=copWrite(cp,offsetof(struct Custom,color[0]),0);
            cp=copWrite(cp,offsetof(struct Custom,color[1]),0);
        }
        *cp++=0xffff;*cp++=0xfffe;
#endif
        paint_banner(gameover_buf, title_str_gameover,
                     sizeof(title_str_gameover) / sizeof(title_str_gameover[0]));
    }
}

typedef enum { BANNER_NONE, BANNER_HEXAGON, BANNER_GAMEOVER, BANNER_LOCKED, BANNER_LOAD_ERROR, BANNER_SAVE_ERROR } Banner;

// Selection keeps its best visible; names are revealed only after unlocking.
// GAME OVER shows for a beat at the start of MODE_GAMEOVER. Either way,
// hud_flash_now() cuts away to the timer HUD with a one-tick white flash.

#pragma GCC pop_options
static Banner banner_active(GameMode m) {
    if(game_ending_complete() || (game_front_visible() && game_front_page()!=3))return BANNER_NONE;
    if (game_load_failed()) return BANNER_LOAD_ERROR;
#if TRACKLOADER
    /* A failed save is a temporary notice, not a replacement menu. Keep
     * persistence/retry state in native_save; hiding this does not mark saved. */
    if (save_failed && m==MODE_ATTRACT) {
        if ((UWORD)(game_mode_timer()-save_failed_at)<3*PC_TICK_RATE)
            return BANNER_SAVE_ERROR;
        save_failed=0;
    }
#endif
    UWORD t = game_mode_timer();
    if (m == MODE_ATTRACT) {
        return BANNER_NONE;
    }
    if (m == MODE_GAMEOVER && t < GAMEOVER_HOLD_TICKS) return BANNER_GAMEOVER;
    return BANNER_NONE;
}

UBYTE hud_flash_now(void) {
    if(game_ending_complete())return 0;
    UWORD t = game_mode_timer();
    switch (game_mode()) {
    case MODE_GAMEOVER:
        return (UBYTE)(t >= GAMEOVER_HOLD_TICKS && t < GAMEOVER_HOLD_TICKS + GAMEOVER_FLASH_TICKS);
    default:
        return 0;
    }
}

void hud_tick(void) {
    GameMode m = game_mode();
    UWORD secs, frames;

    // Current (or just-finished) run while there's one to show, otherwise
    // the best time on record.
    if (m == MODE_PLAYING || m == MODE_DEAD || m == MODE_GAMEOVER || m == MODE_ENDING) {
        secs = gamestate.time_seconds;
        frames = gamestate.time_subsecond_frames;
    } else {
        secs = gamestate.record_seconds;
        frames = gamestate.record_subsecond_frames;
    }
    if (secs > 999) secs = 999;

    if (secs!=glyph_seconds) {
        glyph_seconds=secs;
        cur_glyph[0]=(UBYTE)(secs/100);
        cur_glyph[1]=(UBYTE)((secs/10)%10);
        cur_glyph[2]=(UBYTE)(secs%10);
    }
    UBYTE digits;
    if (frames<FRAME_RATE) {
        digits=centisecond_digits[frames];
    } else {
        // Preserve the old formatting for out-of-contract frame values.
        UWORD cs=(UWORD)muluw(frames,100)/FRAME_RATE;
        if (cs>99) cs=99;
        digits=(UBYTE)(((cs/10)<<4)|(cs%10));
    }
    cur_glyph[SLOT_PERIOD]=GLYPH_PERIOD;
    cur_glyph[4]=digits>>4;
    cur_glyph[5]=digits&15;
}

USHORT* hud_emit_copper(USHORT* copPtr) {
    GameMode mode=game_mode();
    Banner banner = banner_active(mode);
    UBYTE show_timer=!game_front_visible() && !game_ending_complete() && (mode==MODE_ATTRACT || banner==BANNER_NONE);
    UWORD **banner_buf=0;
    if (banner==BANNER_GAMEOVER) banner_buf=gameover_buf;
    else if (banner==BANNER_LOAD_ERROR) banner_buf=load_error_buf;
#if TRACKLOADER
    else if (banner==BANNER_SAVE_ERROR) banner_buf=save_failed==2 ? write_protected_buf : save_error_buf;
#endif
    // Colours must be set before the upper row, not after the multiplex WAIT.
    // Celebratory blink on the digit HUD's 3 colour banks (0-5, the only
    // channels it ever uses) when this run just beat the record. Always
    // emitted - same length every frame regardless of whether it's actually
    // flashing - to keep this tail's total word count frame-invariant, same
    // reasoning as the sprite loops below. Banks stay solid black-on-white
    // any time this isn't true, i.e. every frame outside a fresh GAMEOVER.
    UBYTE flash_on = (UBYTE)(banner == BANNER_NONE && mode == MODE_GAMEOVER
        && !game_ending_complete() && game_new_record()
        && (game_mode_timer() & RECORD_FLASH_PERIOD));
    UWORD fg = flash_on ? 0xfff : 0x000, bg = flash_on ? 0x000 : 0xfff;
    for (WORD i = 0; i < 3; i++) {
        copPtr = copWrite(copPtr, offsetof(struct Custom, color[bank_base[i] + 0]), fg);
        copPtr = copWrite(copPtr, offsetof(struct Custom, color[bank_base[i] + 1]), bg);
    }
    // Two fixed-size pointer batches reuse the channels below the score.
    // Missing allocations use inert descriptors; list length is mode-invariant.
    for (WORD row = 0; row < 2; row++) {
        if (row) {
            // Leave two lines after the upper sprites' stop/terminator fetch.
            // Ten lines remain before the banner starts. Reload POS/CTL
            // explicitly: after the terminator, a new pointer alone does
            // not trigger another DMA control-word fetch.
            copPtr = copWaitY(copPtr, DISPLAY_HW_Y + HUD_Y + HUD_CELL_H + 2);
        }
        for (WORD ch = 0; ch < SPRITE_CHANNELS; ch++) {
            UWORD* buf = (UWORD*)blank_sprite;
            if (!row) {
                if (show_timer
                    && ch < HUD_SLOTS && glyph_buf[ch][cur_glyph[ch]])
                    buf = glyph_buf[ch][cur_glyph[ch]];
            } else if (banner_buf && banner_buf[ch]) {
                buf=banner_buf[ch];
            }
            UWORD offset = (UWORD)(offsetof(struct Custom, sprpt) + ch * sizeof(APTR));
            if (row) {
                // Hardware now receives the header from the copper, so DMA
                // starts at the first pixel pair rather than the header.
                copPtr = copWritePtr(copPtr, offset, buf + 2);
                copPtr = copWrite(copPtr, offsetof(struct Custom, spr[ch].pos), buf[0]);
                copPtr = copWrite(copPtr, offsetof(struct Custom, spr[ch].ctl), buf[1]);
            } else {
                copPtr = copWritePtr(copPtr, offset, buf);
            }
        }
    }
    // Reuse the last pair below the banners. Explicit POS/CTL reload is needed
    // after their terminators, just as for the second HUD row above.
    copPtr=copWaitY(copPtr,DISPLAY_HW_Y+PLAYER_SPRITE_RELOAD_Y);
    for (unsigned ch=0;ch<2;++ch) {
        const UWORD *sprite=render_player_sprite(ch);
        copPtr=copWritePtr(copPtr,offsetof(struct Custom,sprpt)+(ch+6)*sizeof(APTR),(void*)render_player_pixels(ch));
        copPtr=copWrite(copPtr,offsetof(struct Custom,spr[ch+6].pos),sprite[0]);
        copPtr=copWrite(copPtr,offsetof(struct Custom,spr[ch+6].ctl),sprite[1]);
    }
    return copPtr;
}

/* Draw after fill/lines finish into the unpublished foreground buffer. */
void hud_draw_completion(void *buffer) {
    if(!game_ending_complete())return;
    static const UBYTE congratulations[]={TF_C,TF_O,TF_N,TF_G,TF_R,TF_A,TF_T,TF_U,TF_L,TF_A,TF_T,TF_I,TF_O,TF_N,TF_S};
    static const UBYTE complete[]={TF_G,TF_A,TF_M,TF_E,TF_SPACE,TF_C,TF_O,TF_M,TF_P,TF_L,TF_E,TF_T,TF_E};
    UBYTE *plane=buffer;
    static const UBYTE unlocked[]={TF_N,TF_E,TF_W,TF_SPACE,TF_L,TF_E,TF_V,TF_E,TF_L,TF_SPACE,TF_U,TF_N,TF_L,TF_O,TF_C,TF_K,TF_E,TF_D};
    const UBYTE *lines[]={congratulations,game_completion_unlocked()?unlocked:complete};
    const unsigned lengths[]={sizeof(congratulations),game_completion_unlocked()?sizeof(unlocked):sizeof(complete)};
    unsigned top=SCREEN_HEIGHT/2-18;
    for(unsigned y=top-4;y<top+36;++y)
        for(unsigned x=0;x<SCREEN_WIDTH_BYTES;++x)plane[y*SCREEN_WIDTH_BYTES+x]=0;
    for(unsigned line=0;line<2;++line) {
        unsigned left=(SCREEN_WIDTH-(lengths[line]*10-2))/2;
        for(unsigned ch=0;ch<lengths[line];++ch)
            for(unsigned y=0;y<HUD_GLYPH_H;++y)
                for(unsigned x=0;x<4;++x)
                    if(title_font[lines[line][ch]][y]&(8>>x))
                        for(unsigned dy=0;dy<2;++dy)
                            for(unsigned dx=0;dx<2;++dx) {
                                unsigned px=left+ch*10+x*2+dx;
                                unsigned py=top+line*20+y*2+dy;
                                plane[py*SCREEN_WIDTH_BYTES+px/8]|=128>>(px&7);
                            }
    }
}

#pragma GCC push_options
#pragma GCC optimize ("Os")
/* Small planar font for front-end pages; six pixels per character. */
static const UBYTE front_font[26][7]={
 {14,17,17,31,17,17,17},{30,17,17,30,17,17,30},{14,17,16,16,16,17,14},
 {30,17,17,17,17,17,30},{31,16,16,30,16,16,31},{31,16,16,30,16,16,16},
 {14,17,16,23,17,17,15},{17,17,17,31,17,17,17},{14,4,4,4,4,4,14},
 {7,2,2,2,18,18,12},{17,18,20,24,20,18,17},{16,16,16,16,16,16,31},
 {17,27,21,21,17,17,17},{17,25,21,19,17,17,17},{14,17,17,17,17,17,14},
 {30,17,17,30,16,16,16},{14,17,17,17,21,18,13},{30,17,17,30,20,18,17},
 {15,16,16,14,1,1,30},{31,4,4,4,4,4,4},{17,17,17,17,17,17,14},
 {17,17,17,17,17,10,4},{17,17,17,21,21,21,10},{17,17,10,4,10,17,17},
 {17,17,10,4,4,4,4},{31,1,2,4,8,16,31}
};
static void front_text(UBYTE *plane,const char *text,unsigned y,unsigned scale) {
    unsigned len=0;while(text[len])++len;
    if(scale==2) {
        unsigned left=(SCREEN_WIDTH-len*16)/2;
        for(unsigned c=0;c<len;++c) {
            unsigned ch=(unsigned char)text[c];
            if(ch<'A' || ch>'Z')continue;
            for(unsigned row=0;row<16;++row) {
                UWORD bits=menu_font16[ch-'A'][row];
                /* Centred 16-pixel cells always start on a byte boundary. */
                unsigned offset=(y+row)*SCREEN_WIDTH_BYTES+(left>>3)+c*2;
                plane[offset]|=bits>>8;
                plane[offset+1]|=bits;

            }
        }
        return;
    }
    unsigned left=(SCREEN_WIDTH-len*6*scale)/2;
    for(unsigned c=0;c<len;++c)for(unsigned row=0;row<7;++row) {
        unsigned ch=(unsigned char)text[c],bits=0;
        if(ch>='A' && ch<='Z')bits=front_font[ch-'A'][row];
        else if(ch>='0' && ch<='9') {
            static const UBYTE digits[10][7]={{14,17,19,21,25,17,14},{4,12,4,4,4,4,14},{14,17,1,2,4,8,31},{30,1,1,14,1,1,30},{2,6,10,18,31,2,2},{31,16,16,30,1,1,30},{14,16,16,30,17,17,14},{31,1,2,4,8,8,8},{14,17,17,14,17,17,14},{14,17,17,15,1,1,14}};
            bits=digits[ch-'0'][row];
        } else if(ch=='.')bits=row==6?4:0;
        else if(ch==',')bits=row==5?4:row==6?8:0;
        else if(ch==':')bits=(row==2 || row==5)?4:0;
        else if(ch=='/')bits=1u<<(row<5?row:4);
        else if(ch=='-')bits=row==3?14:0;
        else if(ch=='_')bits=row==6?31:0;
        unsigned px=left+c*6;
        UWORD mask=bits<<(11-(px&7));
        unsigned offset=(y+row)*SCREEN_WIDTH_BYTES+(px>>3);
        plane[offset]|=mask>>8;
        plane[offset+1]|=mask;

    }
}
/* Five local scores fit below the unchanged scrolling level title. */
static void front_arcade_scores(UBYTE *plane) {
    const Arcade *a=game_arcade();
    for(unsigned row=0;row<ARCADE_ROWS;++row) {
        const ArcadeScore *score=&a->scores[game_selected_profile()][row];
        char line[]="1. ----------  000.00";
        line[0]+=row;
        if(score->ticks) {
            for(unsigned i=0;i<ARCADE_NAME;++i)line[3+i]=' ';
            for(unsigned i=0;score->name[i] && i<ARCADE_NAME;++i)line[3+i]=score->name[i];
            unsigned sec=score->ticks/60u,cs=score->ticks%60u*100u/60u;
            if(sec>999)sec=999;
            line[15]+=(sec/100);line[16]+=(sec/10)%10;line[17]+=sec%10;
            line[19]+=cs/10;line[20]+=cs%10;
        }
        front_text(plane,line,114+row*11,1);
    }
}
void hud_draw_front(void *buffer) {
    UBYTE *plane=buffer;
    /* Draw only into the unpublished buffer, after its blits have finished. */
    unsigned page=game_front_page();
    if(page==0) {
        blit_wait();
        for(unsigned i=0;i<8000/4;++i)((ULONG*)plane)[i]=0;
    } else {blit_cls(plane);blit_wait();}
    if(page==0) {
        front_text(plane,"SUPER HEXAGON",32,2);
        if(!front_planes) {
            static const char *items[]={"START","OPTIONS","CREDITS"};
            front_text(plane,items[game_front_choice()],92,2);
        }
        front_text(plane,"LEFT / RIGHT TO CHOOSE",158,1);
        front_text(plane,"SPACE / RETURN / FIRE TO SELECT",173,1);
    } else if(page==3) {
        if(!game_selection_locked() && game_selected_profile()>=3)front_text(plane,"HYPER",64,2);
        if(!front_planes) {
            static const char *names[]={"HEXAGON","HEXAGONER","HEXAGONEST"};
            front_text(plane,game_selection_locked()?"LOCKED":names[game_selected_profile()%3],92,2);
        }
        if(game_arcade()->enabled) {
            front_arcade_scores(plane);
            front_text(plane,"ARCADE - FIRE TO START - ESC TO RETURN",180,1);
            return;
        }
        if(!game_selection_locked()) {
        unsigned profile=game_selected_profile();
        char difficulty[]="DIFFICULTY: HARDESTESTESTEST";
        if(profile==1) {difficulty[17]='R';difficulty[18]=0;}
        else difficulty[profile ? 13+3*profile:16]=0;
        front_text(plane,difficulty,120,1);
        char best[]="BEST SCORE: 000.00";
        unsigned seconds=gamestate.record_seconds;
        if(seconds>999)seconds=999;
        best[12]+=(seconds/100);best[13]+=(seconds/10)%10;best[14]+=seconds%10;
        unsigned cs=gamestate.record_subsecond_frames*100u/60u;
        best[16]+=cs/10;best[17]+=cs%10;
        front_text(plane,best,136,1);
        }
        front_text(plane,game_load_failed()?"LOAD ERROR - PRESS FIRE TO RETRY":
            game_selection_locked()?"LOCKED - COMPLETE THE NORMAL LEVEL":"SPACE / RETURN / FIRE TO START",158,1);
        front_text(plane,"LEFT / RIGHT TO CHOOSE - ESC TO RETURN",173,1);
    } else if(page==1) {
        front_text(plane,"OPTIONS",36,2);
        front_text(plane,game_arcade()->enabled?"ARCADE MODE: ON":"ARCADE MODE: OFF",94,1);
        front_text(plane,"SPACE / RETURN / FIRE TO CHANGE",120,1);
        front_text(plane,"ESC TO RETURN",173,1);
    } else if(page==5) {
        front_text(plane,"DISK IS WRITE PROTECTED.",36,1);
        front_text(plane,"NO PROGRESS WILL BE SAVED.",65,1);
        front_text(plane,"IF YOU WANT TO SAVE PROGRESS,",94,1);
        front_text(plane,"REMOVE WRITE PROTECTION NOW.",108,1);
        front_text(plane,"FIRE TO START.",150,1);
    } else if(page==4) {
        const Arcade *a=game_arcade();
        front_text(plane,"HIGH SCORE",32,2);
        front_text(plane,"ENTER YOUR NAME",65,1);
        front_text(plane,a->scores[a->profile][a->row].name,94,1);
        front_text(plane,"TYPE NAME - BACKSPACE TO DELETE",140,1);
        front_text(plane,"RETURN / JOYSTICK FIRE TO ACCEPT",158,1);
        front_text(plane,"ESC TO ACCEPT AND RETURN",173,1);
    } else {
        unsigned page=game_credit_page();
        front_text(plane,"CREDITS",8,2);
        if(page<5) {
            static const char *roles[]={"ORIGINAL GAME CONCEPT AND DESIGN","ORIGINAL SOUNDTRACK","VOICE","FONT - BUMP IT UP","PC PORT"};
            static const char *names[]={"TERRY CAVANAGH","CHIPZEL","JENN FRANK","AARON AMAR - CC BY-SA","ETHAN LEE"};
            unsigned row=page;
            unsigned qr=page<3?page:7-page;
            front_text(plane,roles[row],34,1);front_text(plane,names[row],47,1);
            unsigned n=credit_qr_size[qr],size=(n+8)*2,left=(320-size)/2;
            const UBYTE *bits=credit_qr_bits+credit_qr_offset[qr];
            for(unsigned y=0;y<size;++y)for(unsigned x=0;x<size;++x) {
                unsigned mx=x/2,my=y/2;
                unsigned black=mx>=4 && my>=4 && mx<n+4 && my<n+4 &&
                    (bits[(my-4)*((n+7)/8)+(mx-4)/8] & (128u>>((mx-4)&7)));
                if(!black)plane[(64+y)*40+(left+x)/8]|=128u>>((left+x)&7);
            }
        } else {
            static const char *pages[2][7]={
                {"AMIGA PORT","GOING DIGITAL","ADDITIONAL CODE","A/B - KEIR FRASER","EMMANUEL MARTY","ASTRA - SONNET",""},
                {"PLAYTESTING","AMBROID - ROBINSONB5 - FRIAR","JANK FACTOR - SEIFER - JC - NAG_GRAHAM","PROMETHEUS - RETRO32 - ZENDAR","ADDITIONAL ASSISTANCE","NAG - NORWICH GAMEDEVS","SPAG - AMIGAGAMEDEV"}
            };
            for(unsigned i=0;i<7;++i)front_text(plane,pages[page-5][i],48+i*13,1);
        }
        char counter[]="PAGE 1 / 7";counter[5]+=page;front_text(plane,counter,156,1);
        front_text(plane,"LEFT / RIGHT OR FIRE - ESC TO RETURN",173,1);
    }
}

#pragma GCC pop_options

/* Each bitmap belongs to the matching free copper-list slot. Never repaint
 * the displayed or queued slots; page keys survive visits to gameplay. */
static UWORD front_versions[3];
void *hud_front_bitmap(unsigned slot) {
    if(!game_front_visible() || !front_planes)return 0;
    unsigned locks=game_front_page()==3?game_menu_locks():0;
    ULONG key=game_front_page()==0 ? 0 : game_front_page() |
              ((ULONG)game_credit_page()<<16) | ((ULONG)locks<<20) |
              (game_front_page()==3 ? ((ULONG)game_selected_profile()<<8) |
                ((ULONG)game_selection_locked()<<12) | ((ULONG)game_load_failed()<<13):0);
    UBYTE *plane=front_planes+slot*BITPLANE_SIZE;
    ULONG score=((ULONG)gamestate.record_seconds<<8)|gamestate.record_subsecond_frames;
    UWORD version=game_arcade()->revision;
    unsigned changed=front_versions[slot]!=version || front_keys[slot]!=key || (game_front_page()==3 && front_scores[slot]!=score);
    if(changed) {
        unsigned source=0;
        while(source<3 && (front_versions[source]!=version || front_keys[source]!=key ||
              (game_front_page()==3 && front_scores[source]!=score)))++source;
        if(source<3 && game_front_page()!=0) {
            /* Reading a published bitmap is safe; only this free slot is written. */
            blit_copy_plane(front_planes+source*BITPLANE_SIZE,plane);
            blit_wait();
        } else hud_draw_front(plane);
        front_keys[slot]=key;front_scores[slot]=score;front_versions[slot]=version;
    }
    if(game_front_page()==0 || game_front_page()==3) {
        unsigned levels=game_front_page()==3;
        unsigned strip_key=levels|(locks<<8);
        if(front_strip_page!=strip_key) {
            menu_strip_init_for(front_planes+3*BITPLANE_SIZE,strip_key);
            front_strip_page=strip_key;
        }
        int period=levels?1152:576;
        int position=(int)(levels?game_selected_profile():game_front_choice())*192-game_front_slide();
        if(position<0)position+=period;
        if(position>=period)position-=period;
        if(changed || front_positions[slot]!=position) {
            menu_strip_window(front_planes+3*BITPLANE_SIZE,plane,(unsigned)position);
            front_positions[slot]=position;
        }
    }
    return plane;
}
UBYTE hud_front_cached(void) {return front_planes!=0;}
