#include "hud.h"
#include "config.h"
#include "system.h"  // custom, muluw, AllocMem
#include "coplist.h" // copWritePtr
#include "game.h"
#include "render.h"

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
    TF_H, TF_E, TF_X, TF_A, TF_G, TF_O, TF_N, TF_M, TF_V, TF_R, TF_SPACE, TF_S, TF_T, TF_Y, TF_P, TF_L, TF_C, TF_K, TF_D,
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
};

static const UBYTE title_names[6][16] = {
    {TF_H,TF_E,TF_X,TF_A,TF_G,TF_O,TF_N},
    {TF_H,TF_E,TF_X,TF_A,TF_G,TF_O,TF_N,TF_E,TF_R},
    {TF_H,TF_E,TF_X,TF_A,TF_G,TF_O,TF_N,TF_E,TF_S,TF_T},
    {TF_H,TF_Y,TF_P,TF_E,TF_R,TF_SPACE,TF_H,TF_E,TF_X,TF_A,TF_G,TF_O,TF_N},
    {TF_H,TF_Y,TF_P,TF_E,TF_R,TF_SPACE,TF_H,TF_E,TF_X,TF_A,TF_G,TF_O,TF_N,TF_E,TF_R},
    {TF_H,TF_Y,TF_P,TF_E,TF_R,TF_SPACE,TF_H,TF_E,TF_X,TF_A,TF_G,TF_O,TF_N,TF_E,TF_S,TF_T}
};
static const UBYTE title_lengths[6]={7,9,10,13,15,16};
static const UBYTE title_saving[]={TF_S,TF_A,TF_V,TF_E};
static const UBYTE title_save_error[]={TF_S,TF_A,TF_V,TF_E,TF_SPACE,TF_E,TF_R,TF_R,TF_O,TF_R};
static const UBYTE title_loading[]={TF_L,TF_O,TF_A,TF_D,TF_SPACE,TF_T,TF_R,TF_A,TF_C,TF_K};
static const UBYTE title_load_error[]={TF_L,TF_O,TF_A,TF_D,TF_SPACE,TF_E,TF_R,TF_R,TF_O,TF_R};
static const UBYTE title_locked[]={
    TF_L,
    TF_O,
    TF_C,
    TF_K,
    TF_E,
    TF_D
};
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
static UWORD* locked_buf[SPRITE_CHANNELS];
static UWORD* load_error_buf[SPRITE_CHANNELS];
#if TRACKLOADER
static UWORD* loading_buf[SPRITE_CHANNELS],*saving_buf[SPRITE_CHANNELS],*save_error_buf[SPRITE_CHANNELS];
static UWORD *loading_sprite_pointers;
static UBYTE save_failed;
void hud_save_failed(UBYTE failed) { save_failed=failed; }
/* The native executable, including BSS, resides entirely in Chip RAM. */
static UWORD loading_copper[128];

UWORD* hud_loading_copper(void *plane,UBYTE saving) {
    UWORD *cp=loading_sprite_pointers;
    for (unsigned ch=0;ch<SPRITE_CHANNELS;ch++)
        cp=copWritePtr(cp,offsetof(struct Custom,sprpt)+ch*sizeof(APTR),saving?saving_buf[ch]:loading_buf[ch]);
    copWritePtr(loading_copper,offsetof(struct Custom,bplpt[0]),plane);
    return loading_copper;
}
#endif
static UWORD* title_buf[6][SPRITE_CHANNELS];    // six profile canvases, one slice per channel
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

void hud_free(void) {
    for (WORD slot = 0; slot < HUD_SLOTS; ++slot)
        for (WORD g = 0; g < GLYPH_COUNT; ++g)
            free_sprite(&glyph_buf[slot][g]);
    for (WORD ch = 0; ch < SPRITE_CHANNELS; ++ch) {
        for (WORD p=0;p<6;++p) free_sprite(&title_buf[p][ch]);
        free_sprite(&locked_buf[ch]);
        free_sprite(&load_error_buf[ch]);
#if TRACKLOADER
        free_sprite(&loading_buf[ch]);free_sprite(&saving_buf[ch]);free_sprite(&save_error_buf[ch]);
#endif
        free_sprite(&gameover_buf[ch]);
    }
}

void hud_init(void) {
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
            for (WORD p=0;p<6;++p) title_buf[p][ch]=alloc_canvas_slice(pos,ctl);
            locked_buf[ch]=alloc_canvas_slice(pos,ctl);
            load_error_buf[ch]=alloc_canvas_slice(pos,ctl);
#if TRACKLOADER
            loading_buf[ch]=alloc_canvas_slice(pos,ctl);
            saving_buf[ch]=alloc_canvas_slice(pos,ctl);save_error_buf[ch]=alloc_canvas_slice(pos,ctl);
#endif
            gameover_buf[ch] = alloc_canvas_slice(pos, ctl);
        }
        for (WORD p=0;p<6;++p) paint_banner(title_buf[p],title_names[p],title_lengths[p]);
        paint_banner(locked_buf,title_locked,sizeof(title_locked));
        paint_banner(load_error_buf,title_load_error,sizeof(title_load_error));
#if TRACKLOADER
        paint_banner(loading_buf,title_loading,sizeof(title_loading));
        paint_banner(saving_buf,title_saving,sizeof(title_saving));
        paint_banner(save_error_buf,title_save_error,sizeof(title_save_error));
        UWORD *cp=loading_copper+4; /* plane pointer filled before activation */
        cp=copWrite(cp,offsetof(struct Custom,bplcon0),BPLCON0F_COLOR|BPLCON0F_BPU210);
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
static Banner banner_active(GameMode m) {
    if (game_load_failed()) return BANNER_LOAD_ERROR;
#if TRACKLOADER
    if (save_failed && m!=MODE_PLAYING) return BANNER_SAVE_ERROR;
#endif
    UWORD t = game_mode_timer();
    if (m == MODE_ATTRACT) {
        return game_selection_locked() ? BANNER_LOCKED:BANNER_HEXAGON;
    }
    if (m == MODE_GAMEOVER && t < GAMEOVER_HOLD_TICKS) return BANNER_GAMEOVER;
    return BANNER_NONE;
}

UBYTE hud_flash_now(void) {
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
    if (m == MODE_PLAYING || m == MODE_DEAD || m == MODE_GAMEOVER) {
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
    UBYTE show_timer=mode==MODE_ATTRACT || banner==BANNER_NONE;
    UWORD **banner_buf=0;
    if (banner==BANNER_HEXAGON) banner_buf=title_buf[game_selected_profile()];
    else if (banner==BANNER_LOCKED) banner_buf=locked_buf;
    else if (banner==BANNER_GAMEOVER) banner_buf=gameover_buf;
    else if (banner==BANNER_LOAD_ERROR) banner_buf=load_error_buf;
#if TRACKLOADER
    else if (banner==BANNER_SAVE_ERROR) banner_buf=save_error_buf;
#endif
    // Colours must be set before the upper row, not after the multiplex WAIT.
    // Celebratory blink on the digit HUD's 3 colour banks (0-5, the only
    // channels it ever uses) when this run just beat the record. Always
    // emitted - same length every frame regardless of whether it's actually
    // flashing - to keep this tail's total word count frame-invariant, same
    // reasoning as the sprite loops below. Banks stay solid black-on-white
    // any time this isn't true, i.e. every frame outside a fresh GAMEOVER.
    UBYTE flash_on = (UBYTE)(banner == BANNER_NONE && mode == MODE_GAMEOVER
        && game_new_record()
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
