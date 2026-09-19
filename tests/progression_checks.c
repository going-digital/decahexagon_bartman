#include "../pc_progress.h"
static PcWorld world;
static uint32_t draws;
static uint16_t draw(void *unused,uint16_t bound) {
    (void)unused;
    uint32_t x=(++draws)*0x9e3779b9u;
    x^=x>>16;x*=0x7feb352du;x^=x>>15;
    return (uint16_t)(x%bound);
}
static const struct {
    uint16_t stage,hyper,scenario,chosen,speed,wave,draws,count;
    uint32_t hash;
    uint16_t rotation,shape;
} cases[]={
#include "progression_cases.inc"
};
unsigned pc_progress_checks(void) {
    for (unsigned i=0;i<sizeof(cases)/sizeof(cases[0]);++i) {
        PcProgress p;draws=0;
        pc_progress_reset(&p,&world,(uint8_t)cases[i].stage,(uint8_t)cases[i].hyper,0,draw,0);
        uint32_t start=0;
        p.schedule.shape_counter=5;
        if (cases[i].scenario) {
            start=cases[i].hyper?7198:10798;
            draws=0;p.schedule.wave=80;p.schedule.hyper_entry=0;
            world.speed=33;world.delay_ticks=100;
            p.schedule.rotation_mode=cases[i].hyper?4:0;
            if (cases[i].scenario==3) { start+=7200;p.handoffs=1;p.schedule.stage=1; }
        }
        for (unsigned n=0;n<(cases[i].scenario?3u:1u);++n) {
            pc_world_move(&world);
            pc_progress_tick(&p,&world,6,start+n+1,draw,0);
        }
        uint32_t hash=2166136261u;
        for (unsigned j=0;j<world.count;++j) {
            PcWall *w=&world.walls[j];
            hash=(hash^w->slot)*16777619u;
            hash=(hash^(uint32_t)w->distance)*16777619u;
            hash=(hash^(uint32_t)w->width)*16777619u;
            hash=(hash^w->active)*16777619u;
        }
        if (p.schedule.chosen!=cases[i].chosen || world.speed!=cases[i].speed ||
            p.schedule.wave!=cases[i].wave || draws!=cases[i].draws ||
            world.count!=cases[i].count || hash!=cases[i].hash ||
            p.schedule.rotation_mode!=cases[i].rotation ||
            p.schedule.shape_counter!=cases[i].shape) return 600+i;
    }
    return 0;
}
