#include "../pc_world.h"
#define CHECK(expr) do { if (!(expr)) return 1000 + __LINE__; } while (0)
static PcWorld world;
static const struct {
    uint16_t id,count,delay,wait;
    int16_t base;
    uint16_t draws,roll_count;
    uint32_t digest;
    uint16_t rolls[32];
} cases[] = {
#include "wave_cases.inc"
};
typedef struct { const uint16_t *rolls; uint16_t count,cursor,error; } Tape;
static uint16_t draw(void *context,uint16_t bound) {
    Tape *t=context;
    if (t->cursor>=t->count || t->rolls[t->cursor]>=bound) {t->error=1;return 0;}
    return t->rolls[t->cursor++];
}
unsigned pc_wave_checks(void) {
    for (unsigned c=0;c<sizeof(cases)/sizeof(cases[0]);++c) {
        Tape tape={cases[c].rolls,cases[c].roll_count,0,0};
        pc_world_reset(&world);
        CHECK(pc_generate_wave(&world,cases[c].id,draw,&tape));
        CHECK(!tape.error && tape.cursor==cases[c].draws);
        CHECK(world.count==cases[c].count && world.delay_ticks==cases[c].delay);
        CHECK(world.marker_wait==cases[c].wait && world.spawn_base==cases[c].base);
        uint32_t h=2166136261u;
        for (unsigned i=0;i<world.count;++i) {
            const PcWall *w=&world.walls[i];
            h=(h^w->slot)*16777619u;h=(h^(uint32_t)w->distance)*16777619u;
            h=(h^(uint32_t)w->width)*16777619u;h=(h^w->active)*16777619u;
        }
        CHECK(h==cases[c].digest);
    }
    pc_world_reset(&world);
    CHECK(pc_world_add(&world,0,22,200));
    CHECK(pc_world_add(&world,20,22,0));
    world.marker_wait=1;
    pc_world_move(&world);
    CHECK(world.count==1 && world.walls[0].width==178);
    CHECK(world.morph_state==1 && !world.marker_wait);
    CHECK(pc_world_add(&world,21,22,0));
    CHECK(pc_world_add(&world,22,22,0));
    pc_world_move(&world);
    CHECK(world.morph_state==2 && world.camera_trigger && world.count==1);
    pc_world_reset(&world);
    for (unsigned i=0;i<PC_WALL_CAPACITY;++i) CHECK(pc_world_add(&world,0,4000,200));
    CHECK(!pc_world_add(&world,0,4000,200) && world.overflow);
    world.walls[7].active=0;
    CHECK(pc_world_add(&world,3,1234,500));
    CHECK(world.count==PC_WALL_CAPACITY && world.walls[7].slot==3);
    CHECK(world.walls[7].distance==1234 && world.walls[8].distance==4000);
    return 0;
}
