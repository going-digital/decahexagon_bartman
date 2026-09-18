#include "patterns.h"
#include "config.h"
#include "game.h"

// A pattern is a short sequence of "rows". Each row places walls in a set of
// slots (bitmask, relative to a random anchor slot), then waits `delay` ticks
// before the next row. The walls' inward motion + the row timing is what turns
// a list of rows into a spiral / ladder / etc.
typedef struct { UBYTE slots; UBYTE delay; } PStep;
typedef struct { const PStep* steps; UBYTE count; } Pattern;

// Bit i = slot i. These masks are authored for a 6-slot (hexagon) field
// specifically (0x3F would be "all blocked" - never used) and don't
// generalize to a morphed 4/5-side field - see pick_pattern()'s num_sides!=6
// fallback below.
static const PStep s_ring[]    = { {0x3E, 0} };                       // one gap
static const PStep s_wide_c[]  = { {0x3C, 0} };                       // two-wide gap
static const PStep s_spiral[]  = { {0x01,6},{0x02,6},{0x04,6},{0x08,6},{0x10,6},{0x20,7} };
static const PStep s_rspiral[] = { {0x20,6},{0x10,6},{0x08,6},{0x04,6},{0x02,6},{0x01,7} };
static const PStep s_ladder[]  = { {0x15,10},{0x2A,10},{0x15,10},{0x2A,11} }; // weave 010101/101010
static const PStep s_spingap[] = { {0x3E,11},{0x3D,11},{0x3B,11},{0x37,11},{0x2F,11},{0x1F,12} }; // gap walks round

// The patterns below are modeled on real PC wave shapes documented in
// scratchpad/super_hexagon_pattern_reverse_engineering.md, added after
// playtesting found the original 6 (hand-guessed before that RE work
// existed) didn't feel like the source game. See that report + the approved
// plan for the per-pattern reasoning on what got simplified and why.

// Two opposite-pair "blades" crossing at different times (wave 90, the PC's
// own first wave of a run) - simplified to one shared anchor since this
// engine's pattern format assumes one, where the PC uses two independent
// random anchors.
static const PStep s_pinwheel[] = { {0x09,8}, {0x24,0} };

// Two adjacent-block tiers, closer together than usual (wave 100, the first
// "real" 100s-family wave - the report found it spawns closer than the
// usual baseline, a punchier/lower-warning opener).
static const PStep s_punch[] = { {0x07,6}, {0x18,0} };

// Two near-full rings back to back, gap shifted one slot, short gap between
// (wave 202's tighter/closer-spaced tiers vs. the big multi-thousand-unit
// spreads elsewhere) - reads as a quick double-pulse.
static const PStep s_tight_pulse[] = { {0x3E,4}, {0x3D,0} };

// Extended spingap - same validated gap-walk shape, 8 steps instead of 6
// (wraps past a full loop), tighter per-step delay (the 300s family's
// staircases are structurally an extended version of wave 101/spingap's own
// shape, not a different one - see the report).
static const PStep s_staircase_long[] = {
    {0x3E,9},{0x3D,9},{0x3B,9},{0x37,9},{0x2F,9},{0x1F,9},{0x3E,9},{0x3D,10}
};

static const Pattern P_RING    = { s_ring,    1 };
static const Pattern P_WIDE_C  = { s_wide_c,  1 };
static const Pattern P_SPIRAL  = { s_spiral,  6 };
static const Pattern P_RSPIRAL = { s_rspiral, 6 };
static const Pattern P_LADDER  = { s_ladder,  4 };
static const Pattern P_SPINGAP = { s_spingap, 6 };
static const Pattern P_PINWHEEL       = { s_pinwheel,       2 };
static const Pattern P_PUNCH          = { s_punch,          2 };
static const Pattern P_TIGHT_PULSE    = { s_tight_pulse,    2 };
static const Pattern P_STAIRCASE_LONG = { s_staircase_long, 8 };

// Difficulty-tiered pools, power-of-two sized so the pick is a mask not a %.
// Repeats bias the weighting. pool_early leans toward the wider (2-slot)
// wide_c gap - ring's single-slot gap is a rough first thing to ask of a
// player still learning the controls. Tier placement of the newer,
// PC-wave-modeled patterns mirrors the PC's own family placement (90s=intro,
// 100s=early/mid, 300s=late) rather than being scattered arbitrarily.
static const Pattern* const pool_early[4] = { &P_WIDE_C, &P_WIDE_C, &P_PINWHEEL, &P_RING };
static const Pattern* const pool_mid[8]   = { &P_RING, &P_WIDE_C, &P_SPIRAL, &P_RSPIRAL,
                                              &P_PUNCH, &P_TIGHT_PULSE, &P_LADDER, &P_WIDE_C };
static const Pattern* const pool_late[8]  = { &P_RING, &P_SPIRAL, &P_RSPIRAL, &P_LADDER,
                                              &P_SPINGAP, &P_STAIRCASE_LONG, &P_TIGHT_PULSE, &P_SPINGAP };

// Generic 1- or 2-slot gap, sized to whatever the field's current side count
// is. Used whenever the field isn't a hexagon: the hand-authored patterns
// above assume exactly 6 slots (masks use bits up to bit5) and would
// misfire - wrap to the wrong slots or leave nonsense bits set - on a
// morphed 4/5-side field.
// Currently dead code: num_sides is fixed at 6 for the whole run (the
// side-count morph was removed - it turned out to have no basis in the real
// PC game, see scratchpad/super_hexagon_pattern_reverse_engineering.md).
// Kept anyway as a defensive fallback rather than deleted: cheap, and it's
// exactly the kind of guard the source game itself lacks (its own generator
// can go silently wall-less for a beat on a non-hex field - this can't).
static PStep generic_step; // scratch: filled fresh just before each use
static const Pattern P_GENERIC = { &generic_step, 1 };

static const Pattern* cur;
static UBYTE cur_step;
static UBYTE anchor;
static UWORD timer;

// Count of completed patterns this run - the real PC game's difficulty/family
// tiering keys off a discrete wave-count beat counter, not elapsed seconds
// (scratchpad/super_hexagon_pattern_reverse_engineering.md Part 1 §5), with
// hard checkpoints around wave 12 and wave 30. A decahexagon_bartman "pattern"
// and a PC "wave" aren't quite the same grain, so these are a tuned-by-feel
// starting point at the PC's own checkpoint values, not a precise port.
static UWORD waves_spawned;
#define WAVE_TIER_MID  (12)
#define WAVE_TIER_LATE (30)

UWORD patterns_waves_spawned(void) { return waves_spawned; }

static const Pattern* pick_pattern(void) {
    UWORD w = waves_spawned;
    UWORD r = game_rng();

    if (gamestate.num_sides != 6) {
        UBYTE all = (UBYTE)((1u << gamestate.num_sides) - 1);
        generic_step.slots = (r & 1) ? (UBYTE)(all & ~3u) : (UBYTE)(all & ~1u); // 2-wide or 1-wide gap
        generic_step.delay = 0;
        return &P_GENERIC;
    }
    if (w < WAVE_TIER_MID)       return pool_early[r & 3];
    else if (w < WAVE_TIER_LATE) return pool_mid[r & 7];
    else                         return pool_late[r & 7];
}

// Ticks of calm between patterns, shrinking as the run gets faster. Starts
// at 44 (was 34) for a gentler first few patterns; same floor and shrink
// rate, so it just takes a little longer to reach full pace.
static UWORD inter_gap(void) {
    WORD g = 44 - (WORD)(gamestate.time_seconds / 2);
    return (UWORD)(g < 12 ? 12 : g);
}

static UBYTE wrap_slot(UBYTE s) { return s >= gamestate.num_sides ? (UBYTE)(s - gamestate.num_sides) : s; }

static UBYTE rng_slot(void) {
    return wrap_slot(game_rng() & 7); // 0..7 -> 0..num_sides-1 (num_sides>=4, so one subtraction always suffices)
}

// Anchor for a new pattern, biased toward a slot no OTHER in-flight wall is
// already sitting on. A pattern only guarantees a gap relative to itself;
// rings spawn faster than one fully crosses the field, so 2-3 are normally
// active at once, and purely random anchors can (reliably, in practice)
// leave no slot safe across all of them at once. Falls back to the random
// pick only in the degenerate case where every slot is already occupied.
// This is now the ONLY fairness mechanism in the game: game.c no longer
// blocks turning into an occupied slot (matching the source PC game), so a
// wall can't be "bounced off" - if pick_anchor() ever placed a gap on an
// unsafe slot, that would be an unavoidable hit, not just an inconvenience.
static UBYTE pick_anchor(void) {
    UBYTE start = rng_slot();
    for (UBYTE i = 0; i < gamestate.num_sides; i++) {
        UBYTE s = wrap_slot((UBYTE)(start + i));
        if (!game_slot_blocked(s)) return s;
    }
    return start;
}

static void spawn_step(const PStep* st) {
    UBYTE m = st->slots;
    for (UBYTE i = 0; i < gamestate.num_sides; i++)
        if (m & (1 << i))
            game_spawn_wall(wrap_slot((UBYTE)(anchor + i)), gamestate.wall_spawn_dist);
}

void patterns_reset(void) {
    cur = 0;
    cur_step = 0;
    anchor = 0;
    timer = FRAME_RATE; // ~1s calm before the first pattern of a run
    waves_spawned = 0;
}

void patterns_tick(void) {
    if (timer) { timer--; return; }

    if (!cur) {
        cur = pick_pattern();
        cur_step = 0;
        anchor = pick_anchor();
    }

    spawn_step(&cur->steps[cur_step]);
    UBYTE d = cur->steps[cur_step].delay;
    cur_step++;
    if (cur_step >= cur->count) {
        cur = 0;
        timer = inter_gap();
        waves_spawned++;
    } else {
        timer = d;
    }
}
