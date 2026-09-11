#include "game.h"
#include "config.h"
#include "system.h" // frameCounter (rng entropy)
#include "patterns.h"

#define STARTING_SIDES 6 // hexagon; must match levels[0].num_sides below

GameState gamestate = {
    .field_angle = 0,
    .field_rotation = 500,
    .segment_angle = ((65536 + STARTING_SIDES - 1) / STARTING_SIDES), // Ensure overflow after last segment
    .segment_angle_target = ((65536 + STARTING_SIDES - 1) / STARTING_SIDES),
    .num_sides = STARTING_SIDES,
    .player_angle = 0,
    .wall_fraction = 0,
    .draw_distance = ZOOM_ONE,
    .draw_distance_target = ZOOM_ONE,
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
#define PLAYER_TURN_RATE (1400)               // angle units / tick (~0.13 slots/tick on a hexagon; slower per-slot on morphed, wider-slotted fields)

// Side-count morphs as a run gets harder: hexagon -> pentagon -> square.
// Each entry's fields apply once gamestate.time_seconds reaches start_seconds.
typedef struct { UBYTE num_sides; UWORD start_seconds; } LevelDef;
#define NUM_LEVELS 3
static const LevelDef levels[NUM_LEVELS] = {
    { STARTING_SIDES, 0 },
    { 5,              20 },
    { 4,              40 },
};
static UBYTE cur_level;

// Placeholder beat: free-running, not synced to the music yet (Phase 4).
#define BEAT_BPM         (132)
#define BEAT_PERIOD      (FRAME_RATE * 60 / BEAT_BPM)

static GameMode mode;
static UWORD mode_timer;      // ticks elapsed in the current mode
static WORD  wall_speed;      // px / tick, ramps with time
static WORD  shake_x, shake_y;
static UBYTE new_record;      // this run beat the previous best - latches until reset_run()

static UWORD beat_ctr;       // counts up to BEAT_PERIOD
static UWORD beat_env;       // decays after each beat; drives the zoom pulse
static UBYTE on_beat;        // 1 for the single tick a beat lands
static UWORD zoom_base;      // Q8 resting zoom, eases toward draw_distance_target
static WORD  field_rot_target;
static UWORD rot_timer;      // ticks until the next rotation-speed change

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

// Free-running beat + camera zoom breathing. Runs in every mode.
static void update_ambient(void) {
    if (++beat_ctr >= BEAT_PERIOD) {
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

static void pick_rotation(void) {
    UWORD r = rng();
    WORD mag = 250 + (r & 0x1ff);              // 250..761 units/tick
    field_rot_target = (r & 0x200) ? mag : -mag; // sometimes reverses
    rot_timer = (FRAME_RATE * 2) + (rng() & 0xff); // change again in ~2..7s
}

static void clear_walls(void) {
    for (WORD i = 0; i < MAX_WALLS; i++) walls[i].active = 0;
}

// Deliberately plain C math, NOT muluw(): with muluw() here, GCC's
// -flto -fwhole-program optimizer silently eliminated the ENTIRE caller-side
// blocking check below (confirmed by disassembly - the whole
// `if (new_slot != old_slot && wall_overlaps_radius(...)) new_angle =
// old_angle;` block vanished, leaving player_angle updated unconditionally).
// muluw's inline asm has no "memory" clobber, so LTO loses track of the
// dependency on walls[]/gamestate through it and mis-proves the branch dead.
// Costs one __mulsi3 call/tick here - correctness over that micro-cost.
static UWORD angle_to_slot(UWORD angle) {
    return (UWORD)(((ULONG)angle * gamestate.num_sides) >> 16);
}

// Is any active wall in `slot` currently spanning `radius`? Shared by the
// rotation blocker (side-on contact: can't turn into a wall) and the death
// check (radial contact: a wall's inside edge sweeping inward catches you).
static UBYTE wall_overlaps_radius(UBYTE slot, WORD radius) {
    for (WORD i = 0; i < MAX_WALLS; i++) {
        if (!walls[i].active || walls[i].slot != slot) continue;
        WORD d = walls[i].dist;
        if (d <= radius && d + WALL_THICKNESS >= radius) return 1;
    }
    return 0;
}

// ceil(65536/sides). Plain C division, not divuw() - only called on a level
// transition (a handful of times per run, nowhere near hot-path), and given
// angle_to_slot()'s muluw() just turned out to cause a real miscompilation
// under this build's -flto (see its comment), there's no reason to keep
// divuw()'s equivalent risk here for a call this infrequent.
static UWORD segment_angle_for(UBYTE sides) {
    return (UWORD)((65536u + sides - 1u) / sides);
}

// Slot boundaries move when the side count changes, so old walls' slot
// indices no longer mean the same angular position - clear them rather than
// let them jump to the wrong place (or momentarily alias a different slot).
static void enter_level(UBYTE idx) {
    cur_level = idx;
    gamestate.num_sides = levels[idx].num_sides;
    gamestate.segment_angle = gamestate.segment_angle_target = segment_angle_for(gamestate.num_sides);
    clear_walls();
}

static void update_difficulty(void) {
    UWORD t = gamestate.time_seconds;
    wall_speed = 2 + (WORD)(t / 15);
    if (wall_speed > 5) wall_speed = 5;

    UBYTE idx = cur_level;
    while (idx + 1 < NUM_LEVELS && t >= levels[idx + 1].start_seconds) idx++;
    if (idx != cur_level) enter_level(idx);
}

static void reset_run(void) {
    gamestate.field_angle = 0;
    gamestate.field_rotation = 500;
    gamestate.player_angle = 0;
    gamestate.wall_fraction = 0;
    cur_level = 0;
    gamestate.num_sides = levels[0].num_sides;
    gamestate.segment_angle = gamestate.segment_angle_target = segment_angle_for(gamestate.num_sides);
    gamestate.draw_distance_target = ZOOM_ONE;
    gamestate.draw_distance = ZOOM_ONE;
    gamestate.time_seconds = 0;
    gamestate.time_subsecond_frames = 0;
    clear_walls();
    patterns_reset();
    wall_speed = 2;
    shake_x = shake_y = 0;
    new_record = 0;
    zoom_base = ZOOM_ONE;
    beat_ctr = beat_env = 0;
    field_rot_target = gamestate.field_rotation;
    rot_timer = FRAME_RATE * 3;
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

    // Rotation: ease toward a target that changes every few seconds (and can flip).
    if (--rot_timer == 0) pick_rotation();
    gamestate.field_rotation += (field_rot_target - gamestate.field_rotation) >> 4;

    // Slowly zoom the view out as the run gets faster.
    {
        WORD zt = ZOOM_ONE - (WORD)(gamestate.time_seconds);
        if (zt < 216) zt = 216;
        gamestate.draw_distance_target = (UWORD)zt;
    }

    // Rotating into a wall's side just blocks the turn - it's only lethal
    // when its inside edge sweeps inward and reaches you (checked below).
    // PLAYER_TURN_RATE is well under one slot/tick, so at most one slot
    // boundary is ever crossed here.
    {
        UWORD old_angle = gamestate.player_angle;
        UWORD new_angle = (UWORD)(old_angle + (UWORD)(in->turn * PLAYER_TURN_RATE));
        UWORD old_slot = angle_to_slot(old_angle);
        UWORD new_slot = angle_to_slot(new_angle);
        if (new_slot != old_slot && wall_overlaps_radius((UBYTE)new_slot, PLAYER_RADIUS))
            new_angle = old_angle; // blocked: hold at the boundary
        gamestate.player_angle = new_angle;
    }

    for (WORD i = 0; i < MAX_WALLS; i++) {
        if (!walls[i].active) continue;
        walls[i].dist -= wall_speed;
        if (walls[i].dist <= HUB_RADIUS) walls[i].active = 0;
    }

    patterns_tick();

    // Death: a wall's inside edge has swept inward onto the player's slot.
    // (Side-on contact from rotating into a wall was already blocked above.)
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
