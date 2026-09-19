#include "../pc_progress.h"
#include <stdio.h>
static PcWorld world;
static uint32_t seed,draws;
static uint16_t draw(void *unused,uint16_t bound) {
    (void)unused;
    uint32_t x=seed+(++draws)*0x9e3779b9u;
    x^=x>>16;x*=0x7feb352du;x^=x>>15;
    return (uint16_t)(x%bound);
}
int main(void) {
    for(unsigned stage=0;stage<3;stage++) for(unsigned hyper=0;hyper<2;hyper++)
    for(unsigned scenario=0;scenario<4;scenario++) for(seed=0;seed<16;seed++) {
        if ((stage==2 && scenario) || (scenario==3 && stage!=0)) continue;
        PcProgress p;draws=0;
        pc_progress_reset(&p,&world,(uint8_t)stage,(uint8_t)hyper,0,draw,0);
        unsigned start=scenario ? (hyper ? 7198:10798):0;
        p.schedule.shape_counter=5;
        if(scenario) {
            draws=0;p.schedule.wave=80;p.schedule.hyper_entry=0;
            world.speed=33;p.schedule.rotation_mode=hyper?4:0;
            world.delay_ticks=scenario>=2?100:0;
            if(scenario==3) { start+=7200;p.handoffs=1;p.schedule.stage=1; }
        }
        for(unsigned n=0;n<8;n++) {
            unsigned tick=start+n+1;
            pc_world_move(&world);
            pc_progress_tick(&p,&world,6,tick,draw,0);
            uint32_t hash=2166136261u;
            for(unsigned j=0;j<world.count;j++) {
                PcWall *w=&world.walls[j];
                hash=(hash^w->slot)*16777619u;
                hash=(hash^(uint32_t)w->distance)*16777619u;
                hash=(hash^(uint32_t)w->width)*16777619u;
                hash=(hash^w->active)*16777619u;
            }
            int next1=(stage==0 && p.handoffs) ? 1:-1;
            int next2=(p.schedule.stage==2 && stage!=2) ? 2:-1;
            printf("%u %u %u %u %u %u %u %u %u %u %u %u %d %d %u %u %u %u",stage,hyper,scenario,seed,tick,
                stage,hyper,tick,world.speed,world.delay_ticks,(unsigned)p.schedule.wave,p.schedule.hyper_entry,
                next2,next1,p.schedule.stage==2?9:7,p.schedule.rotation_mode,(unsigned)draws,p.schedule.chosen>=0);
            if(p.schedule.chosen>=0) printf(" %d",p.schedule.chosen);
            printf(" %u %u %u\n",world.count,(unsigned)hash,p.schedule.shape_counter);
        }
    }
}
