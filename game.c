#include "game.h"
#include "pc_morph.h"
#include "system.h"
#include "patterns.h"
#include "pc_menu.h"
#include "pc_lifecycle.h"
#if CHEAT_MODE
#include "cheat.h"
#endif
#ifdef MUSIC_LSP
#include "audio.h"
#endif

#define STARTING_ZOOM_TARGET 128
#define ATTRACT_ZOOM_TARGET (ZOOM_ONE * 50 / HUB_RADIUS)
#define FALLBACK_BEAT_BPM 132
GameState gamestate;
PcWorld game_world;
PcPalette game_palette;
static PcPlayer player;
static PcMorph morph;
static PcMenu menu;
static PcRecords records;
static PcLifecycle lifecycle;
static UBYTE selected_profile;
UBYTE game_selected_profile(void) { return selected_profile; }
UBYTE game_selection_locked(void) { return !pc_profile_unlocked(&records,selected_profile); }
static void load_record(void) {
    uint32_t best=records.best[selected_profile];
    gamestate.record_seconds=(UWORD)(best/60);
    gamestate.record_subsecond_frames=(UWORD)(best%60);
}
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
#if CHEAT_MODE
    cheat_reset();
#endif
    pc_palette_start(&game_palette,selected_profile%3,selected_profile/3);
    pc_world_reset(&game_world);pc_morph_reset(&morph);patterns_reset();
    pc_lifecycle_start(&lifecycle,&player);
    project_state();
    gamestate.field_angle=0;gamestate.field_rotation=182;
    gamestate.draw_distance_target=STARTING_ZOOM_TARGET;
    gamestate.time_seconds=gamestate.time_subsecond_frames=0;
    shake_x=shake_y=0;new_record=0;beat_ctr=beat_env=0;
}
void game_init(void) {
    pc_lifecycle_init(&lifecycle);
    player.angle=player.previous_angle=30;
    selected_profile=PC_START_STAGE+3*PC_START_HYPER;
    pc_menu_reset(&menu,selected_profile);reset_run();set_mode(MODE_ATTRACT);
}

static void record_time(void) {
    uint32_t elapsed=(uint32_t)gamestate.time_seconds*60+gamestate.time_subsecond_frames;
    if (pc_record_tick(&records,selected_profile,elapsed)) new_record=1;
    load_record();
}

static void update_playing(const InputState *in) {
    pc_lifecycle_tick(&lifecycle);
    gamestate.time_seconds=(UWORD)(lifecycle.elapsed/60);
    gamestate.time_subsecond_frames=(UWORD)(lifecycle.elapsed%60);
    record_time();
    UBYTE held=in->held;
#if CHEAT_MODE
    if (in->cheat_held) held=cheat_steer(&player,&game_world,morph.sides,patterns_turn_rate());
    else cheat_reset();
#endif
    player.angle=pc_turn(player.angle,held,patterns_turn_rate());
    pc_morph_tick(&morph,&game_world);
    project_state();
    /* Collision sees pre-motion distances and can restore the prior angle.
     * The authoritative record ordering is never changed for rendering. */
    pc_collide(&player,game_world.walls,game_world.count,morph.sides,(int16_t)game_world.speed);
    project_state();
    if (player.hit || game_world.overflow) {
        pc_lifecycle_die(&lifecycle);set_mode(MODE_DEAD);return;
    }
    pc_world_move(&game_world);
    patterns_tick();
    if (patterns_transitioned()) {
        pc_morph_reset(&morph);project_state();
        pc_palette_enter(&game_palette,patterns_stage(),1);
    }
#if PC_CORE_SELFTEST
    /* First-wave travel invariant independently checks the live clock/store
     * connection on 68000, not just isolated portable functions. */
    uint32_t tick=(uint32_t)gamestate.time_seconds*60+gamestate.time_subsecond_frames;
    const int32_t opening_speed=selected_profile>=3 ? (selected_profile%3==2 ? 40:33):
        (selected_profile%3==2 ? 35:selected_profile%3==1 ? 24:22);
    const int32_t opening_distance=selected_profile>=3 ? (selected_profile%3==2 ? 4375:3950):
        (selected_profile%3==2 ? 4050:selected_profile%3==1 ? 3435:3300);
    if (tick<=80 && (game_world.speed!=opening_speed || game_world.count==0 ||
        game_world.walls[0].distance!=opening_distance-opening_speed*(int32_t)(tick-1))) live_failure=1;
#endif
    /* Temporary planar rotation response. The scheduler consumes the exact
     * source RNG draws; 3D tilt/cue effects remain outstanding. */
    static const WORD rotations[]={182,-182,364,-364,546,-546,728,-728,910,-910};
    gamestate.field_rotation=rotations[patterns_rotation_mode()];
}

static void start_playing(const InputState *in) {
    InputState first=*in;
    first.held=0; /* input already ran in selection/death before PC restart */
#if CHEAT_MODE
    first.cheat_held=0;
#endif
    reset_run();set_mode(MODE_PLAYING);
    pc_palette_tick(&game_palette,patterns_effective_score(1));
    update_playing(&first);
}

void game_update(const InputState* in) {
    mode_timer++;
    update_ambient();
    pc_palette_tick(&game_palette,mode==MODE_PLAYING ?
        patterns_effective_score((uint32_t)gamestate.time_seconds*FRAME_RATE+gamestate.time_subsecond_frames+1) : 0);

    // Escape abandons a run / backs out to the title. From the title itself
    // main.c turns Escape into a quit.
    if (in->back_edge && mode != MODE_ATTRACT) {
        reset_run();
        pc_menu_reset(&menu,selected_profile);
        pc_lifecycle_init(&lifecycle);
        set_mode(MODE_ATTRACT);
        gamestate.field_angle += gamestate.field_rotation;
        return;
    }

    switch (mode) {
    case MODE_ATTRACT:
        // Keep the menu pointer visible; ease to gameplay zoom on confirmation.
        gamestate.draw_distance_target = ATTRACT_ZOOM_TARGET;
        pc_menu_tick(&menu,in->held);
        if (selected_profile!=pc_menu_profile(&menu)) {
            selected_profile=pc_menu_profile(&menu);load_record();
            pc_palette_start(&game_palette,selected_profile%3,selected_profile/3);
            mode_timer=0;
        }
        player.angle=player.previous_angle=menu.angle;project_state();
        if (in->fire_edge && !game_selection_locked()) {
            rng_state ^= (UWORD)frameCounter | 1u;
            if (!rng_state) rng_state=0x2545;
            start_playing(in);
        }
        break;

    case MODE_PLAYING:
        update_playing(in);
        break;

    case MODE_DEAD:
    case MODE_GAMEOVER:
        /* PC input runs before the transition tick: a held confirmation
         * retries on the first tick whose incoming extent is already 320. */
        if ((in->fire || in->fire_edge) && pc_lifecycle_can_start(&lifecycle)) {
            start_playing(in);
        } else {
            pc_lifecycle_tick(&lifecycle);
            if (mode==MODE_DEAD && pc_lifecycle_can_start(&lifecycle))
                set_mode(MODE_GAMEOVER);
        }
        break;
    }

    // The field never stops turning, in any mode.
    gamestate.field_angle += gamestate.field_rotation;
}
