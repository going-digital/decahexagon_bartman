#pragma once
#include <exec/types.h>
#include "input.h"
#include "config.h"
#include "pc_world.h"
#include "pc_palette.h"

/* PC walls project at hub+distance/5. Camera-scaled direction tables match
 * the PC's untilted baseline view; zoom remains a relative camera control. */
#define HUB_RADIUS 40
#define PLAYER_RADIUS 54
#define ZOOM_ONE 256
#define GAMEPLAY_ZOOM 128

typedef struct {
    UWORD field_angle;
    WORD field_rotation;
    UWORD segment_angle;
    UBYTE num_sides;
    UWORD player_angle;
    UWORD draw_distance,draw_distance_target;
    UWORD pulse; /* PC radial displacement, before camera scaling. */
    UWORD time_seconds,time_subsecond_frames;
    UWORD record_seconds,record_subsecond_frames;
} GameState;
extern GameState gamestate;
extern PcWorld game_world;
extern PcPalette game_palette;

typedef enum { MODE_ATTRACT,MODE_PLAYING,MODE_DEAD,MODE_GAMEOVER,MODE_ENDING } GameMode;
void game_init(void);
void game_update(const InputState *in);
UWORD game_rng(void);
GameMode game_mode(void);
UWORD game_mode_timer(void);
WORD game_shake_x(void);
WORD game_shake_y(void);
UBYTE game_new_record(void);

#if PC_CORE_SELFTEST
UWORD game_live_failure(void);
#endif

UBYTE game_selected_profile(void);
UBYTE game_selection_locked(void);

/* Optional resident-platform hook, called synchronously before starting a run.
 * Return nonzero only with the selected profile's audio ready. Failure leaves
 * selection/death state intact. Called on retries too; platform may reuse a bank.
 * Install after game_init. Never call game_update from the hook. */
typedef int (*GameRunPreparer)(UBYTE profile);
void game_set_run_preparer(GameRunPreparer prepare);
/* Consume after game_update: discard accumulated ticks and rebase frame time
 * whenever preparation ran, including failed loads. */
UBYTE game_take_load_barrier(void);

/* Retained until a successful retry, selection change or return to the menu. */
UBYTE game_load_failed(void);

/* Storage boundary: restore immediately after game_init, before game_update.
 * Snapshot only at menu/results; caller must stop audio before disk I/O.
 * Commit acknowledgement is allowed ONLY after verified disk readback. */
#include "trackloader/save.h"
int game_restore_save(const TrackSave *state);
int game_save_snapshot(TrackSave *state);
int game_save_committed(const TrackSave *state);
UBYTE game_save_dirty(void);

UBYTE game_ending_complete(void);
