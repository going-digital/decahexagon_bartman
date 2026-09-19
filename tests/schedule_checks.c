#include "../pc_schedule.h"
#include "../pc_morph.h"
#include "../pc_projection.h"
#define CHECK(e) do { if (!(e)) return 2000+__LINE__; } while (0)
static PcWorld world;
static PcSpan spans[PC_WALL_CAPACITY];
static uint32_t seed,draws;
static uint16_t draw(void *unused,uint16_t bound) {
    (void)unused;
    uint32_t x=seed+(++draws)*0x9e3779b9u;
    x^=x>>16;x*=0x7feb352du;x^=x>>15;
    return (uint16_t)(x%bound);
}
static const struct {
    uint16_t stage,wave,sides,score,shape,seed;
    int16_t chosen;
    uint16_t next_wave,next_shape,speed,delay,wait;
} cases[]={
#include "schedule_cases.inc"
};
unsigned pc_schedule_checks(void) {
    PcSchedule s;
    for (unsigned i=0;i<sizeof(cases)/sizeof(cases[0]);++i) {
        pc_world_reset(&world);pc_schedule_reset(&s);
        s.stage=(uint8_t)cases[i].stage;s.wave=cases[i].wave;s.shape_counter=cases[i].shape;
        seed=cases[i].seed;draws=0;
        pc_schedule_tick(&s,&world,(uint8_t)cases[i].sides,cases[i].score+1,draw,0);
        CHECK(s.chosen==cases[i].chosen && s.wave==cases[i].next_wave);
        CHECK(s.shape_counter==cases[i].next_shape && world.speed==cases[i].speed);
        CHECK(world.delay_ticks==cases[i].delay && world.marker_wait==cases[i].wait);
    }
    /* Stage2 late-phase entry precedes even a non-expired wave delay. It
     * consumes one draw, clears records once, and preserves delay/speed. */
    pc_world_reset(&world);pc_schedule_reset(&s);s.stage=2;s.wave=51;
    world.speed=35;world.delay_ticks=4;world.delay_numerator=140;
    pc_world_add(&world,0,500,200);seed=draws=0;
    pc_schedule_tick(&s,&world,6,7200,draw,0);
    CHECK(!s.late_phase && world.count==1 && draws==0 && world.delay_ticks==3);
    pc_schedule_tick(&s,&world,6,7201,draw,0);
    CHECK(s.late_phase && world.count==0 && !world.walls[0].active);
    CHECK(draws==1 && world.speed==35 && world.delay_ticks==2 && world.delay_numerator==140);
    CHECK(s.rotation_mode>=8 && s.rotation_mode<=9 && world.camera_trigger);
    pc_world_add(&world,1,500,200);
    pc_schedule_tick(&s,&world,6,7202,draw,0);
    CHECK(draws==1 && world.count==1 && world.delay_ticks==1);
    pc_schedule_tick(&s,&world,6,7203,draw,0);
    CHECK(world.speed==40 && s.wave==52);
    /* Wave-dependent visual requests are retained independently of selection. */
    for (unsigned wave=1;wave<=55;++wave) {
        pc_world_reset(&world);pc_schedule_reset(&s);s.stage=2;s.wave=wave;
        seed=draws=0;pc_schedule_tick(&s,&world,6,1,draw,0);
        CHECK(s.flip_request==(wave==12 || wave==24 || wave==48));
        if (wave==5) CHECK(s.rotation_mode>=2 && s.rotation_mode<=3);
        if (wave>5 && wave%4==0) CHECK(s.rotation_mode>=4 && s.rotation_mode<=5);
    }
    /* Native visual routine: shrink completes on tick11; growth on tick12.
     * A directly entered decay state at zero clears immediately. */
    for (uint8_t trigger=1;trigger<=5;++trigger) {
        PcMorph m={5,0,0};pc_world_reset(&world);world.morph_state=trigger;
        for (unsigned tick=1;tick<=13;++tick) {
            pc_morph_tick(&m,&world);
            if (trigger==1) {
                CHECK(m.sides==(tick<11 ? 5:4));
                CHECK(world.morph_state==(tick<11 ? 1:0));
            } else if (trigger==2 || trigger==4) {
                CHECK(m.sides==6 && world.morph_state==(tick<12 ? trigger+1:0));
                if (tick==1) CHECK(pc_morph_arc(&m)==72);
                if (tick==12) CHECK(pc_morph_arc(&m)==60);
            } else CHECK(m.sides==5 && !world.morph_state);
        }
    }
    /* Markers freeze the existing delay until consumed; the consumption tick
     * resumes countdown, and newly spawned walls wait until the next move. */
    pc_world_reset(&world);pc_schedule_reset(&s);seed=draws=0;
    world.marker_wait=1;world.delay_ticks=2;
    pc_world_add(&world,20,44,0);
    pc_world_move(&world);pc_schedule_tick(&s,&world,6,1,draw,0);
    CHECK(world.delay_ticks==2 && s.wave==0);
    pc_world_move(&world);pc_schedule_tick(&s,&world,6,2,draw,0);
    CHECK(world.delay_ticks==1 && !world.marker_wait && world.morph_state==1);
    pc_schedule_tick(&s,&world,6,3,draw,0);
    CHECK(s.chosen==90 && world.count>0 && world.walls[0].distance==3300);
    /* Separate truncation, same-slot overlap union, and marker exclusion. */
    pc_world_reset(&world);
    pc_world_add(&world,0,19,19);pc_world_add(&world,0,0,25);
    pc_world_add(&world,1,0,25);pc_world_add(&world,22,0,400);
    CHECK(pc_project_spans(&world,6,spans)==2);
    CHECK(spans[0].slot==0 && spans[0].inner==40 && spans[0].outer==46);
    CHECK(spans[1].slot==1 && spans[1].outer==45);
    CHECK(world.count==4 && world.walls[0].distance==19);
    pc_world_reset(&world);
    pc_world_add(&world,0,3300,200);
    CHECK(pc_project_spans(&world,6,spans)==1);
    CHECK(spans[0].inner==700 && spans[0].outer==740);
    return 0;
}
