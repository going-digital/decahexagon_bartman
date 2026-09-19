#include "../pc_progress.h"
#include "../pc_morph.h"
#include <stdio.h>
static PcWorld world;
static uint32_t rng;
static uint16_t draw(void *unused,uint16_t bound) {
    (void)unused;
    rng^=rng<<13;rng^=rng>>17;rng^=rng<<5;
    return (uint16_t)(rng%bound);
}
int main(void) {
    for (unsigned stage=0;stage<3;++stage) for (unsigned hyper=0;hyper<2;++hyper) {
        unsigned peak=0,total_handoffs=0;
        for (unsigned seed=1;seed<=64;++seed) {
            PcProgress p;PcMorph m;rng=seed;
            pc_progress_reset(&p,&world,(uint8_t)stage,(uint8_t)hyper,0,draw,0);
            pc_morph_reset(&m);
            /* Forced survival is a storage/order stress test, not a complete
             * native gameplay replay. Stop before the unimplemented ending. */
            for (uint32_t tick=1;tick<=25200;++tick) {
                pc_morph_tick(&m,&world);pc_world_move(&world);
                pc_progress_tick(&p,&world,m.sides,tick,draw,0);
                if (p.transitioned) {
                    ++total_handoffs;pc_morph_reset(&m);
                    if (p.schedule.wave!=52 || p.schedule.hyper_entry || world.marker_wait) return 1;
                    if (p.schedule.chosen!=(int)(92+p.schedule.stage)) return 2;
                    if (pc_progress_score(&p,tick)!=3601) return 3;
                }
                if (world.overflow || m.sides<3 || m.sides>6 || m.phase>11) return 4;
                if (world.count>peak) peak=world.count;
                if (tick==1 && p.schedule.chosen!=(int)(90+stage+3*hyper)) return 5;
                if (p.schedule.stage==2 && pc_progress_score(&p,tick)>=7200) break;
                if (tick==25200) return 6;
            }
            if (p.handoffs!=2-stage || p.schedule.stage!=2) return 7;
        }
        printf("Stage%u hyper%u: 64 runs, %u handoffs, peak %u records\n",stage,hyper,total_handoffs,peak);
    }
    return 0;
}
