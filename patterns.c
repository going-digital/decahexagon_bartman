#include "patterns.h"
#include "game.h"
#include "pc_progress.h"

static PcProgress progress;
static uint16_t random_draw(void *unused,uint16_t bound) {
    (void)unused;
    /* xorshift16 visits 1..65535. Reject the incomplete bucket instead of
     * biasing anchors by taking a mask or raw modulo. */
    uint16_t limit=(uint16_t)(65535u-(65535u%bound)),r;
    do { r=(uint16_t)(game_rng()-1); } while (r>=limit);
    return (uint16_t)(r%bound);
}
void patterns_reset(void) {
    uint8_t old=progress.schedule.rotation_mode;
    pc_progress_reset(&progress,&game_world,game_selected_profile()%3,game_selected_profile()/3,old,random_draw,0);
}
void patterns_tick(void) {
    uint32_t score=(uint32_t)gamestate.time_seconds*60+gamestate.time_subsecond_frames;
    pc_progress_tick(&progress,&game_world,gamestate.num_sides,score,random_draw,0);
}
UWORD patterns_wave_count(void) { return (UWORD)progress.schedule.wave; }
UBYTE patterns_rotation_mode(void) { return progress.schedule.rotation_mode; }

UBYTE patterns_stage(void) { return progress.schedule.stage; }
UBYTE patterns_transitioned(void) { return progress.transitioned; }
UBYTE patterns_turn_rate(void) { return progress.schedule.stage==2 ? 9:7; }
uint32_t patterns_effective_score(uint32_t elapsed) { return pc_progress_score(&progress,elapsed); }
