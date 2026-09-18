#include "game.h"
#include "config.h"
#include "system.h" // frameCounter (rng entropy)
#include "patterns.h"
#ifdef MUSIC_LSP
#include "audio.h" // audio_get_bpm() - real beat sync, see update_ambient()
#endif

#define STARTING_SIDES 6 // hexagon - field side count is fixed for the whole run (see num_sides in game.h)
#define STARTING_WALL_SPEED 1 // must match update_difficulty()'s/reset_run()'s starting wall_speed below
// Visible range, in seconds of travel at the current wall_speed - see
// update_difficulty()'s comment. 3/2 (not a plain literal) so this stays a
// compile-time-constant multiply/divide-by-power-of-2 wherever it's used on
// a runtime wall_speed, same reasoning as FRAME_RATE itself.
#define VIEW_SECONDS_NUM 3
#define VIEW_SECONDS_DEN 2
#define STARTING_WALL_SPAWN_DIST (HUB_RADIUS + STARTING_WALL_SPEED * FRAME_RATE * VIEW_SECONDS_NUM / VIEW_SECONDS_DEN)
// Zoom so a freshly-spawned wall renders right at the screen edge - see
// update_difficulty()'s comment. All-constant expression, safe to compute
// here at compile time.
#define STARTING_ZOOM_TARGET (ZOOM_ONE * SCREEN_EDGE_RADIUS / STARTING_WALL_SPAWN_DIST)

// Idle attract-screen close-up on the hub: HUB_RADIUS maps to ~80 screen px
// (comfortably under SCREEN_EDGE_RADIUS, leaving a margin). Set continuously
// while in MODE_ATTRACT (below); the existing zoom_base easing in
// update_ambient() (every tick, every mode) tweens draw_distance smoothly
// toward this on the way in, and back toward STARTING_ZOOM_TARGET (reset by
// reset_run() right before leaving ATTRACT) on the way out - no separate
// transition/tween logic needed, just setting the right target per mode.
#define ATTRACT_ZOOM_TARGET (ZOOM_ONE * 80 / HUB_RADIUS)

GameState gamestate = {
    .field_angle = 0,
    .field_rotation = 500,
    .segment_angle = ((65536 + STARTING_SIDES - 1) / STARTING_SIDES), // Ensure overflow after last segment
    .segment_angle_target = ((65536 + STARTING_SIDES - 1) / STARTING_SIDES),
    .num_sides = STARTING_SIDES,
    .wall_thickness = (STARTING_WALL_SPEED * FRAME_RATE / 10),  // ~100ms of travel
    .wall_spawn_dist = STARTING_WALL_SPAWN_DIST,
    .player_angle = 0,
    .wall_fraction = 0,
    .draw_distance = STARTING_ZOOM_TARGET,
    .draw_distance_target = STARTING_ZOOM_TARGET,
    .time_seconds = 0,
    .time_subsecond_frames = 0,
    .record_seconds = 0,
    .record_subsecond_frames = 0,
};

Wall walls[MAX_WALLS];

// Ticks are logic frames. TODO: on NTSC the loop runs at 60Hz, so a "second"
// here is 0.83s wall-clock - Phase 6 normalises difficulty to real time.
#define READY_TICKS      (FRAME_RATE * 3 / 2) // ~1.5s "BEGIN" lead-in
#define DEAD_TICKS       (FRAME_RATE)         // ~1s hit freeze
// Two selectable turn models - see config.h's CONTROL_FLICK for why both are
// kept in the tree rather than one replacing the other.
#if CONTROL_FLICK
// Turning is a discrete, fixed-duration slot-to-slot "flick" (matches the
// source PC game - see scratchpad/super_hexagon_pattern_reverse_engineering.md
// Part 7 §7.1), not continuous rotation: press a direction, the player angle
// sweeps at a constant rate for FLICK_TICKS ticks, then hard-snaps to the
// destination slot's exact center. FLICK_TICKS is the PC's own value.
#define FLICK_TICKS (10)
#else
#define PLAYER_TURN_RATE (1400) // angle units / tick (~0.13 slots/tick on the hexagon field)
#endif

// Beat period, in ticks. Synced to the actual music's BPM when MUSIC_LSP is
// built in (LSP tracks its live BPM - see audio_get_bpm() - and can change
// tempo mid-song, which this follows); FALLBACK_BEAT_BPM is only used
// without a music backend (or before the very first tick, in practice never
// observed since p61Init() always runs before the main loop starts).
#define FALLBACK_BEAT_BPM (132)

// Wall speed ramps in fine (1/8-unit) steps on a cheap tick counter, then the
// actual per-tick pixel movement is smoothed further via a Bresenham-style
// fractional accumulator (wall_speed_accum, in update_playing()) - together
// these approximate the source PC game's continuous "+dt every tick" ramp
// (scratchpad/super_hexagon_pattern_reverse_engineering.md Part 3 §3.1)
// using only adds/shifts/masks, never a per-tick multiply or divide. The
// three DERIVED quantities below (thickness/spawn_dist/zoom) still only
// recompute when the WHOLE unit changes (same "a handful of times per run,
// now ~32 instead of 4" cadence as before) - they don't need per-tick
// precision, unlike the raw movement rate.
#define WALL_SPEED_CAP_X8 (5 * 8) // same cap (5) as before, in eighths
// Ticks between each 1/8-unit increase - matches the old "+1 whole unit
// every 15s" overall ramp time (15*FRAME_RATE ticks per whole unit / 8
// eighths per unit), compile-time constant, no runtime division.
#define WALL_SPEED_RAMP_TICKS ((15 * FRAME_RATE) / 8)

static GameMode mode;
static UWORD mode_timer;      // ticks elapsed in the current mode
static UWORD wall_speed_x8 = STARTING_WALL_SPEED * 8; // fixed-point eighths
static UWORD wall_ramp_ctr;   // ticks until the next 1/8-unit speed increase
static UWORD wall_speed_accum; // fractional-pixel carry for the per-tick move
static WORD  wall_speed;      // last-recomputed WHOLE unit of wall_speed_x8 - gates the derived recompute below, not read for movement any more
static WORD  shake_x, shake_y;
static UBYTE new_record;      // this run beat the previous best - latches until reset_run()

static UWORD beat_ctr;       // counts up to beat_period
static UWORD beat_bpm = FALLBACK_BEAT_BPM;
static UWORD beat_period = (FRAME_RATE * 60 / FALLBACK_BEAT_BPM); // ticks/beat, recomputed only when beat_bpm actually changes
static UWORD beat_env;       // decays after each beat; drives the zoom pulse
static UBYTE on_beat;        // 1 for the single tick a beat lands
// Q8 resting zoom, eases toward draw_distance_target. Seeded here (not by
// reset_run(), which only moves the target) so the very first frame is
// already at the right zoom instead of easing up from 0.
static UWORD zoom_base = STARTING_ZOOM_TARGET;
// Field rotation kicks are milestone-triggered (waves_spawned crossing a
// threshold), not a free-running timer - matching the source PC game's
// otisrotate(), which only ever re-targets rotation speed at difficulty
// milestones, easing toward it the rest of the time (Part 2 §2.2). Magnitude
// alternates direction each kick rather than rerolling randomly, also
// matching otisrotate(). Reuses patterns.c's wave-tier milestones as the
// nearest equivalent this codebase has to the PC's own rank-up thresholds.
#define ROTATION_MILD_MAG    (300) // eased forward spin at run start (PC: ease to +10)
#define ROTATION_STRONG_MAG  (700) // kick magnitude at each milestone (PC: alternating +-20)
#define ROTATION_KICK_WAVE_1 (12)  // first kick - matches patterns.c's WAVE_TIER_MID
#define ROTATION_KICK_WAVE_2 (30)  // second kick - matches patterns.c's WAVE_TIER_LATE
#define ROTATION_KICK_WAVE_STEP (20) // repeat cadence after the second kick
static WORD  field_rot_target;
static UWORD next_rot_kick_wave; // next patterns_waves_spawned() milestone that kicks rotation
static UBYTE rot_kick_negative;  // alternates each kick

#if CONTROL_FLICK
// Flick state: 0 = at rest between flicks; else counts toward 0 (sign = direction).
// flick_step (angle moved per tick while mid-flick) is cached once per run by
// reset_run() - it's segment_angle/FLICK_TICKS, and segment_angle only ever
// changes on a run reset now that the side-count morph is gone (game.c history).
static WORD  flick_timer;
static UWORD flick_step;
#endif

// 16-bit xorshift PRNG.
static UWORD rng_state = 0x2545;
static UWORD rng(void) {
    rng_state ^= rng_state << 7;
    rng_state ^= rng_state >> 9;
    rng_state ^= rng_state << 8;
    return rng_state;
}

static void set_mode(GameMode m) {
    mode = m;
    mode_timer = 0;
}

GameMode game_mode(void)      { return mode; }
UWORD    game_mode_timer(void) { return mode_timer; }
WORD     game_shake_x(void)   { return shake_x; }
WORD     game_shake_y(void)   { return shake_y; }
UBYTE    game_on_beat(void)   { return on_beat; }
UWORD    game_rng(void)       { return rng(); }
UBYTE    game_new_record(void) { return new_record; }

void game_spawn_wall(UBYTE slot, WORD dist) {
    for (WORD i = 0; i < MAX_WALLS; i++) {
        if (!walls[i].active) {
            walls[i].active = 1;
            walls[i].slot = slot;
            walls[i].dist = dist;
            walls[i].width = gamestate.wall_thickness; // frozen at spawn - see Wall.width's comment
            return;
        }
    }
}

// Does ANY active wall (at any distance) currently occupy `slot`? Each
// pattern only guarantees a gap relative to itself - with rings spawning
// faster than a ring's ~66-tick spawn-to-hub transit, 2-3 rings are normally
// in flight at once, and if their gaps land on uncoordinated random slots,
// their combined coverage can leave no slot safe at all. patterns.c uses
// this to steer a new pattern's gap onto a slot no other in-flight wall is
// already sitting on, whenever one exists.
UBYTE game_slot_blocked(UBYTE slot) {
    for (WORD i = 0; i < MAX_WALLS; i++)
        if (walls[i].active && walls[i].slot == slot) return 1;
    return 0;
}

// Beat-driven camera zoom breathing. Runs in every mode.
static void update_ambient(void) {
#ifdef MUSIC_LSP
    // Gated on an actual change (not every tick) so the division stays off
    // the hot path - same reasoning as wall_speed's gated recompute above.
    // 0 is never a real BPM; guards a torn/pre-init read rather than latch a
    // bogus period.
    UWORD live_bpm = audio_get_bpm();
    if (live_bpm != 0 && live_bpm != beat_bpm) {
        beat_bpm = live_bpm;
        beat_period = (UWORD)(FRAME_RATE * 60 / beat_bpm);
    }
#endif
    if (++beat_ctr >= beat_period) {
        beat_ctr = 0;
        on_beat = 1;
        beat_env = 220;
    } else {
        on_beat = 0;
    }
    beat_env -= beat_env >> 3; // exp decay, ~half-life 5 ticks
    if (beat_env < 8) beat_env = 0;

    zoom_base += ((WORD)gamestate.draw_distance_target - (WORD)zoom_base) >> 3;
    gamestate.draw_distance = zoom_base + (beat_env >> 2); // beat pops the view toward the camera
}

static void rotation_kick(void) {
    field_rot_target = (WORD)(rot_kick_negative ? -ROTATION_STRONG_MAG : ROTATION_STRONG_MAG);
    rot_kick_negative = !rot_kick_negative;
    next_rot_kick_wave = (next_rot_kick_wave == ROTATION_KICK_WAVE_1)
        ? ROTATION_KICK_WAVE_2
        : (UWORD)(next_rot_kick_wave + ROTATION_KICK_WAVE_STEP);
}

static void clear_walls(void) {
    for (WORD i = 0; i < MAX_WALLS; i++) walls[i].active = 0;
}

// Deliberately plain C math, NOT muluw(): with muluw() here, GCC's
// -flto -fwhole-program optimizer previously eliminated an entire
// wall-dependent conditional built on this function's result (confirmed by
// disassembly on an earlier version of this code, back when this fed a
// turn-blocking check) - muluw's inline asm has no "memory" clobber, so LTO
// loses track of the dependency on walls[]/gamestate through it and
// mis-proves the branch dead. The same risk applies to the death check below
// that still consumes this function's result. Costs one __mulsi3 call/tick
// here - correctness over that micro-cost.
static UWORD angle_to_slot(UWORD angle) {
    return (UWORD)(((ULONG)angle * gamestate.num_sides) >> 16);
}

// Is any active wall in `slot` currently spanning `radius`? Used by the death
// check: a wall's inside edge sweeping inward onto the player's radius.
static UBYTE wall_overlaps_radius(UBYTE slot, WORD radius) {
    for (WORD i = 0; i < MAX_WALLS; i++) {
        if (!walls[i].active || walls[i].slot != slot) continue;
        WORD d = walls[i].dist;
        if (d <= radius && d + walls[i].width >= radius) return 1;
    }
    return 0;
}

// ceil(65536/sides). Plain C division, not divuw() - only called once per run
// (reset_run()), nowhere near hot-path, and given angle_to_slot()'s muluw()
// just turned out to cause a real miscompilation under this build's -flto
// (see its comment), there's no reason to keep divuw()'s equivalent risk here
// for a call this infrequent.
static UWORD segment_angle_for(UBYTE sides) {
    return (UWORD)((65536u + sides - 1u) / sides);
}

#if CONTROL_FLICK
// Angle at the centre of `slot`. Only called once per flick completion (every
// FLICK_TICKS ticks), nowhere near hot-path - muluw() here is for consistency
// with angle_to_slot()'s -flto lesson (see its comment), not a perf need.
static UWORD slot_center_angle(UWORD slot) {
    return (UWORD)(muluw(slot, gamestate.segment_angle) + (gamestate.segment_angle >> 1));
}
#endif

static void update_difficulty(void) {
    if (wall_speed_x8 < WALL_SPEED_CAP_X8 && ++wall_ramp_ctr >= WALL_SPEED_RAMP_TICKS) {
        wall_ramp_ctr = 0;
        wall_speed_x8++;
    }
    WORD new_speed = (WORD)(wall_speed_x8 >> 3); // whole units, for the derived-value gate only

    if (new_speed != wall_speed) {
        wall_speed = new_speed;
        // Both in TIME, not fixed distance, so the visual read (gap around a
        // wall, reaction time) stays constant as wall_speed ramps - see
        // game.h. wall_speed*FRAME_RATE is a runtime value times a
        // compile-time constant, and /10 divides by one too - both safe, no
        // lib-math risk.
        gamestate.wall_thickness = (WORD)(wall_speed * FRAME_RATE / 10);
        gamestate.wall_spawn_dist =
            (WORD)(HUB_RADIUS + wall_speed * FRAME_RATE * VIEW_SECONDS_NUM / VIEW_SECONDS_DEN);

        // Zoom so a freshly-spawned wall (at wall_spawn_dist) always renders
        // right at the screen edge, regardless of wall_speed - otherwise a
        // slow start (small wall_spawn_dist) leaves most of the screen
        // empty, and a fast run's much larger wall_spawn_dist would spawn
        // walls off-screen. Plain '/' here is fine: this whole block only
        // runs when wall_speed actually changes (a handful of times per
        // run), nowhere near hot-path.
        gamestate.draw_distance_target =
            (UWORD)(ZOOM_ONE * SCREEN_EDGE_RADIUS / gamestate.wall_spawn_dist);
    }
}

static void reset_run(void) {
    gamestate.field_angle = 0;
    gamestate.field_rotation = 500;
    gamestate.wall_fraction = 0;
    gamestate.num_sides = STARTING_SIDES;
    gamestate.segment_angle = gamestate.segment_angle_target = segment_angle_for(gamestate.num_sides);
#if CONTROL_FLICK
    // At rest on slot 0's centre, not the raw-0 slot BOUNDARY - flicks always
    // end on a slot centre (see slot_center_angle()), so the resting state
    // should start there too.
    gamestate.player_angle = slot_center_angle(0);
    flick_timer = 0;
    flick_step = (UWORD)(gamestate.segment_angle / FLICK_TICKS);
#else
    gamestate.player_angle = 0;
#endif
    // Only the TARGET changes here - draw_distance/zoom_base are left alone
    // so update_ambient()'s existing per-tick ease (every mode, every tick)
    // carries the zoom there smoothly instead of snapping. Matters most for
    // the attract screen's close-up -> gameplay transition on fire-press.
    gamestate.draw_distance_target = STARTING_ZOOM_TARGET;
    gamestate.time_seconds = 0;
    gamestate.time_subsecond_frames = 0;
    clear_walls();
    patterns_reset();
    wall_speed_x8 = STARTING_WALL_SPEED * 8;
    wall_ramp_ctr = 0;
    wall_speed_accum = 0;
    wall_speed = STARTING_WALL_SPEED;
    gamestate.wall_thickness = (WORD)(wall_speed * FRAME_RATE / 10);
    gamestate.wall_spawn_dist = STARTING_WALL_SPAWN_DIST;
    shake_x = shake_y = 0;
    new_record = 0;
    beat_ctr = beat_env = 0;
    field_rot_target = ROTATION_MILD_MAG; // mild forward ease-in, matching the PC's run-start behaviour
    next_rot_kick_wave = ROTATION_KICK_WAVE_1;
    rot_kick_negative = 0;
}

void game_init(void) {
    reset_run();
    set_mode(MODE_ATTRACT);
}

static void record_time(void) {
    UWORD s = gamestate.time_seconds, f = gamestate.time_subsecond_frames;
    if (s > gamestate.record_seconds ||
        (s == gamestate.record_seconds && f > gamestate.record_subsecond_frames)) {
        gamestate.record_seconds = s;
        gamestate.record_subsecond_frames = f;
        new_record = 1;
    }
}

static void update_playing(const InputState* in) {
    if (++gamestate.time_subsecond_frames >= FRAME_RATE) {
        gamestate.time_subsecond_frames = 0;
        gamestate.time_seconds++;
    }
    update_difficulty();

    // Rotation: ease toward a target that only re-targets at wave-count
    // milestones (rotation_kick()), not on a free-running timer.
    if (patterns_waves_spawned() >= next_rot_kick_wave) rotation_kick();
    gamestate.field_rotation += (field_rot_target - gamestate.field_rotation) >> 4;

    // Zoom target is set in update_difficulty() (above), keyed to
    // wall_spawn_dist so it tracks wall_speed instead of drifting on a
    // fixed schedule of its own.

    // Turning is unconditional either way - it's never blocked by an
    // occupied wall slot (the PC's input handler never references the wall
    // array at all); death is decided purely by the radial-overlap check
    // below. Which of the two models below is compiled in is config.h's
    // CONTROL_FLICK.
#if CONTROL_FLICK
    // Discrete slot-to-slot "flick", matching the source PC game (confirmed
    // by decompile - see FLICK_TICKS's comment): a new flick only starts at
    // rest (flick_timer==0), so holding a direction auto-repeats one flick
    // right after another rather than free-spinning.
    if (flick_timer == 0 && in->turn != 0)
        flick_timer = (WORD)(in->turn > 0 ? FLICK_TICKS : -FLICK_TICKS);

    if (flick_timer > 0) {
        gamestate.player_angle = (UWORD)(gamestate.player_angle + flick_step);
        if (--flick_timer == 0)
            gamestate.player_angle = slot_center_angle(angle_to_slot(gamestate.player_angle));
    } else if (flick_timer < 0) {
        gamestate.player_angle = (UWORD)(gamestate.player_angle - flick_step);
        if (++flick_timer == 0)
            gamestate.player_angle = slot_center_angle(angle_to_slot(gamestate.player_angle));
    }
#else
    // Continuous free rotation at a constant rate - this project's original
    // control model, kept as the fallback (see config.h's CONTROL_FLICK).
    gamestate.player_angle = (UWORD)(gamestate.player_angle + (UWORD)(in->turn * PLAYER_TURN_RATE));
#endif

    // Bresenham-style fractional carry: over several ticks this averages out
    // to exactly wall_speed_x8/8 px/tick, using only an add/shift/mask - see
    // wall_speed_x8's comment for why (cheap per-tick smoothing of a ramp
    // that itself only updates in coarse steps, no multiply or divide here).
    wall_speed_accum = (UWORD)(wall_speed_accum + wall_speed_x8);
    WORD move = (WORD)(wall_speed_accum >> 3);
    wall_speed_accum &= 7;
    for (WORD i = 0; i < MAX_WALLS; i++) {
        if (!walls[i].active) continue;
        if (walls[i].dist > HUB_RADIUS) {
            walls[i].dist -= move;
            if (walls[i].dist <= HUB_RADIUS) walls[i].dist = HUB_RADIUS; // hold the leading edge at the hub, don't overshoot past it
        } else {
            // Leading edge is already at the hub - the trailing edge (see
            // Wall.width) keeps sweeping in until it catches up, then the
            // wall deactivates. Two-stage despawn, see Wall.width's comment.
            walls[i].width -= move;
            if (walls[i].width <= 0) walls[i].active = 0;
        }
    }

    patterns_tick();

    // Death: a wall's inside edge has swept inward onto the player's slot.
    UWORD pslot = angle_to_slot(gamestate.player_angle);
    if (wall_overlaps_radius((UBYTE)pslot, PLAYER_RADIUS)) {
        record_time();
        set_mode(MODE_DEAD);
    }
}

void game_update(const InputState* in) {
    mode_timer++;
    update_ambient();

    // Escape abandons a run / backs out to the title. From the title itself
    // main.c turns Escape into a quit.
    if (in->back_edge && mode != MODE_ATTRACT) {
        reset_run();
        set_mode(MODE_ATTRACT);
        gamestate.field_angle += gamestate.field_rotation;
        return;
    }

    switch (mode) {
    case MODE_ATTRACT:
        // Idle close-up on the hub; eases back out via reset_run() below the
        // instant fire is pressed (see ATTRACT_ZOOM_TARGET's comment).
        gamestate.draw_distance_target = ATTRACT_ZOOM_TARGET;
        if (in->fire_edge) {
            rng_state ^= (UWORD)frameCounter | 1u; // seed entropy from run start
            reset_run();
            set_mode(MODE_READY);
        }
        break;

    case MODE_READY:
        if (mode_timer >= READY_TICKS) set_mode(MODE_PLAYING);
        break;

    case MODE_PLAYING:
        update_playing(in);
        break;

    case MODE_DEAD:
        if (mode_timer < 8) {
            shake_x = (WORD)(rng() & 7) - 3;
            shake_y = (WORD)(rng() & 7) - 3;
        } else {
            shake_x = shake_y = 0;
        }
        if (mode_timer >= DEAD_TICKS) set_mode(MODE_GAMEOVER);
        break;

    case MODE_GAMEOVER:
        if (in->fire_edge) {
            reset_run();
            set_mode(MODE_READY);
        }
        break;
    }

    // The field never stops turning, in any mode.
    gamestate.field_angle += gamestate.field_rotation;
}
