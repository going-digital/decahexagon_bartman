#include <assert.h>
#include <stdio.h>
#include "../cheat.h"
#include "../pc_schedule.h"
#include "../pc_morph.h"
static PcWorld world;
static uint32_t rng;
static uint16_t draw(void *unused,uint16_t bound) {
    (void)unused;
    rng^=rng<<13;rng^=rng>>17;rng^=rng<<5;
    return (uint16_t)(rng%bound);
}
int main(void) {
    /* Exercise the real scheduler/collision path, without forced survival.
     * This is an assist regression, not a desktop replay or perfect-play claim. */
    for (unsigned stage=0;stage<3;++stage) {
        unsigned minimum=3600,total=0;
        for (unsigned seed=1;seed<=32;++seed) {
            PcPlayer player={30,30,0,0};PcSchedule schedule;PcMorph morph;
            pc_world_reset(&world);pc_schedule_reset(&schedule);pc_morph_reset(&morph);
            cheat_reset();schedule.stage=(uint8_t)stage;rng=seed;
            uint8_t rate=stage==2 ? 9:7;
            unsigned tick;
            for (tick=1;tick<=3600;++tick) {
                player.angle=pc_turn(player.angle,cheat_steer(&player,&world,morph.sides,rate),rate);
                pc_morph_tick(&morph,&world);
                pc_collide(&player,world.walls,world.count,morph.sides,world.speed);
                if (player.hit) break;
                pc_world_move(&world);
                pc_schedule_tick(&schedule,&world,morph.sides,tick,draw,0);
                assert(!world.overflow);
            }
            if (tick>3600) tick=3600;
            if (tick<minimum) minimum=tick;
            total+=tick;
        }
        assert(minimum>=600); /* Every seed must navigate the opening waves. */
        printf("Assist stage%u: minimum %.2fs, mean %.2fs (60s cap, 32 seeds)\n",
               stage,minimum/60.,total/32./60.);
    }
    return 0;
}
