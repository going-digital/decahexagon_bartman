#include "patterns.h"
#include "config.h"
#include "game.h"

// A pattern is a short sequence of "rows". Each row places walls in a set of
// slots (bitmask, relative to a random anchor slot), then waits `delay` ticks
// before the next row. The walls' inward motion + the row timing is what turns
// a list of rows into a spiral / ladder / etc.
typedef struct { UBYTE slots; UBYTE delay; } PStep;
typedef struct { const PStep* steps; UBYTE count; } Pattern;

// Bit i = slot i. NUM_SIDES is 6, so 0x3F would be "all blocked" - never used.
static const PStep s_ring[]    = { {0x3E, 0} };                       // one gap
static const PStep s_wide_c[]  = { {0x3C, 0} };                       // two-wide gap
static const PStep s_spiral[]  = { {0x01,6},{0x02,6},{0x04,6},{0x08,6},{0x10,6},{0x20,7} };
static const PStep s_rspiral[] = { {0x20,6},{0x10,6},{0x08,6},{0x04,6},{0x02,6},{0x01,7} };
static const PStep s_ladder[]  = { {0x15,10},{0x2A,10},{0x15,10},{0x2A,11} }; // weave 010101/101010
static const PStep s_spingap[] = { {0x3E,11},{0x3D,11},{0x3B,11},{0x37,11},{0x2F,11},{0x1F,12} }; // gap walks round

static const Pattern P_RING    = { s_ring,    1 };
static const Pattern P_WIDE_C  = { s_wide_c,  1 };
static const Pattern P_SPIRAL  = { s_spiral,  6 };
static const Pattern P_RSPIRAL = { s_rspiral, 6 };
static const Pattern P_LADDER  = { s_ladder,  4 };
static const Pattern P_SPINGAP = { s_spingap, 6 };

// Difficulty-tiered pools, power-of-two sized so the pick is a mask not a %.
// Repeats bias the weighting.
static const Pattern* const pool_early[4] = { &P_RING, &P_WIDE_C, &P_RING, &P_WIDE_C };
static const Pattern* const pool_mid[8]   = { &P_RING, &P_WIDE_C, &P_SPIRAL, &P_RSPIRAL,
                                              &P_RING, &P_SPIRAL, &P_LADDER, &P_WIDE_C };
static const Pattern* const pool_late[8]  = { &P_RING, &P_SPIRAL, &P_RSPIRAL, &P_LADDER,
                                              &P_SPINGAP, &P_SPIRAL, &P_LADDER, &P_SPINGAP };

static const Pattern* cur;
static UBYTE cur_step;
static UBYTE anchor;
static UWORD timer;

static const Pattern* pick_pattern(void) {
    UWORD t = gamestate.time_seconds;
    UWORD r = game_rng();
    if (t < 10)      return pool_early[r & 3];
    else if (t < 25) return pool_mid[r & 7];
    else             return pool_late[r & 7];
}

// Ticks of calm between patterns, shrinking as the run gets faster.
static UWORD inter_gap(void) {
    WORD g = 34 - (WORD)(gamestate.time_seconds / 2);
    return (UWORD)(g < 12 ? 12 : g);
}

static UBYTE wrap_slot(UBYTE s) { return s >= NUM_SIDES ? (UBYTE)(s - NUM_SIDES) : s; }

static UBYTE rng_slot(void) {
    return wrap_slot(game_rng() & 7); // 0..7 -> 0..NUM_SIDES-1
}

static void spawn_step(const PStep* st) {
    UBYTE m = st->slots;
    for (UBYTE i = 0; i < NUM_SIDES; i++)
        if (m & (1 << i))
            game_spawn_wall(wrap_slot((UBYTE)(anchor + i)), WALL_SPAWN_DIST);
}

void patterns_reset(void) {
    cur = 0;
    cur_step = 0;
    anchor = 0;
    timer = FRAME_RATE; // ~1s calm before the first pattern of a run
}

void patterns_tick(void) {
    if (timer) { timer--; return; }

    if (!cur) {
        cur = pick_pattern();
        cur_step = 0;
        anchor = rng_slot();
    }

    spawn_step(&cur->steps[cur_step]);
    UBYTE d = cur->steps[cur_step].delay;
    cur_step++;
    if (cur_step >= cur->count) {
        cur = 0;
        timer = inter_gap();
    } else {
        timer = d;
    }
}
