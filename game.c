#include "game.h"
#include "pc_morph.h"
#include "system.h"
#include "patterns.h"
#ifdef MUSIC_LSP
#include "audio.h"
#endif

#define STARTING_ZOOM_TARGET 128
#define ATTRACT_ZOOM_TARGET (ZOOM_ONE * 80 / HUB_RADIUS)
#define READY_TICKS (FRAME_RATE * 3 / 2)
#define DEAD_TICKS FRAME_RATE
#define FALLBACK_BEAT_BPM 132
GameState gamestate;
PcWorld game_world;
static PcPlayer player;
static PcMorph morph;
#if PC_CORE_SELFTEST
static UWORD live_failure;
UWORD game_live_failure(void) { return live_failure; }
#endif
static GameMode mode;
static UWORD mode_timer;
static WORD shake_x,shake_y;
static UBYTE new_record,on_beat;
static UWORD beat_ctr,beat_env,beat_bpm=FALLBACK_BEAT_BPM;
static UWORD beat_period=FRAME_RATE*60/FALLBACK_BEAT_BPM;
static UWORD zoom_base=STARTING_ZOOM_TARGET;
static UWORD rng_state=0x2545;
static UWORD rng(void) {
    rng_state ^= rng_state << 7;
    rng_state ^= rng_state >> 9;
    rng_state ^= rng_state << 8;
    return rng_state;
}
static void set_mode(GameMode m) { mode=m;mode_timer=0; }
GameMode game_mode(void) { return mode; }
UWORD game_mode_timer(void) { return mode_timer; }
WORD game_shake_x(void) { return shake_x; }
WORD game_shake_y(void) { return shake_y; }
UBYTE game_on_beat(void) { return on_beat; }
UWORD game_rng(void) { return rng(); }
UBYTE game_new_record(void) { return new_record; }

/* Existing presentation/audio placeholders, not PC cue or camera emulation. */
// Beat-driven camera zoom breathing. Runs in every mode.
static void update_ambient(void) {
#ifdef MUSIC_LSP
    // Recompute only when the music tempo changes.
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

static void project_state(void) {
    gamestate.num_sides=morph.sides;
    gamestate.segment_angle=pc_render_angle((int16_t)pc_morph_arc(&morph));
    gamestate.player_angle=pc_render_angle(player.angle ? 360-player.angle:0);
}
static void reset_run(void) {
    pc_world_reset(&game_world);pc_morph_reset(&morph);patterns_reset();
    player.angle=player.previous_angle=30;player.hit=player.blocked=0;
    project_state();
    gamestate.field_angle=0;gamestate.field_rotation=182;
    gamestate.draw_distance_target=STARTING_ZOOM_TARGET;
    gamestate.time_seconds=gamestate.time_subsecond_frames=0;
    shake_x=shake_y=0;new_record=0;beat_ctr=beat_env=0;
}
void game_init(void) { reset_run();set_mode(MODE_ATTRACT); }

static void record_time(void) {
    UWORD s = gamestate.time_seconds, f = gamestate.time_subsecond_frames;
    if (s > gamestate.record_seconds ||
        (s == gamestate.record_seconds && f > gamestate.record_subsecond_frames)) {
        gamestate.record_seconds = s;
        gamestate.record_subsecond_frames = f;
        new_record = 1;
    }
}

static void update_playing(const InputState *in) {
    if (++gamestate.time_subsecond_frames>=FRAME_RATE) {
        gamestate.time_subsecond_frames=0;++gamestate.time_seconds;
    }
    player.angle=pc_turn(player.angle,in->held,7);
    pc_morph_tick(&morph,&game_world);
    project_state();
    /* Collision sees pre-motion distances and can restore the prior angle.
     * The authoritative record ordering is never changed for rendering. */
    pc_collide(&player,game_world.walls,game_world.count,morph.sides,(int16_t)game_world.speed);
    project_state();
    if (player.hit || game_world.overflow) {
        record_time();set_mode(MODE_DEAD);return;
    }
    pc_world_move(&game_world);
    patterns_tick();
#if PC_CORE_SELFTEST
    /* First-wave travel invariant independently checks the live clock/store
     * connection on 68000, not just isolated portable functions. */
    uint32_t tick=(uint32_t)gamestate.time_seconds*60+gamestate.time_subsecond_frames;
    if (tick<=80 && (game_world.speed!=22 || game_world.count==0 ||
        game_world.walls[0].distance!=3300-22*(int32_t)(tick-1))) live_failure=1;
#endif
    /* Temporary planar rotation response. The scheduler consumes the exact
     * source RNG draws; 3D tilt/cue and stage progression remain outstanding. */
    static const WORD rotations[]={182,-182,364,-364,546,-546};
    gamestate.field_rotation=rotations[patterns_rotation_mode()];
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
            rng_state ^= (UWORD)frameCounter | 1u;
            if (!rng_state) rng_state=0x2545;
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
