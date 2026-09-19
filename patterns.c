#include "patterns.h"
#include "game.h"
#include "pc_schedule.h"

static PcSchedule schedule;
static uint16_t random_draw(void *unused,uint16_t bound) {
    (void)unused;
    /* xorshift16 visits 1..65535. Reject the incomplete bucket instead of
     * biasing anchors by taking a mask or raw modulo. */
    uint16_t limit=(uint16_t)(65535u-(65535u%bound)),r;
    do { r=(uint16_t)(game_rng()-1); } while (r>=limit);
    return (uint16_t)(r%bound);
}
void patterns_reset(void) { pc_schedule_reset(&schedule); schedule.stage=PC_START_STAGE; }
void patterns_tick(void) {
    uint32_t score=(uint32_t)gamestate.time_seconds*60+gamestate.time_subsecond_frames;
    pc_schedule_tick(&schedule,&game_world,gamestate.num_sides,score,random_draw,0);
}
UWORD patterns_wave_count(void) { return (UWORD)schedule.wave; }
UBYTE patterns_rotation_mode(void) { return schedule.rotation_mode; }
