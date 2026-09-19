#include <assert.h>
#include <stdio.h>
#include "../cheat.h"
static PcWorld world;

static void corridor(uint8_t opening,uint8_t expected,uint8_t sides,uint8_t rate,uint16_t speed) {
    pc_world_reset(&world);cheat_reset();world.speed=speed;
    for (uint8_t s=0;s<sides;++s)
        if (s!=opening) pc_world_add(&world,s,650,200);
    PcPlayer p={30,30,0,0};
    assert(cheat_steer(&p,&world,sides,rate)==expected);
    assert(p.angle==30 && world.walls[0].distance==650);
    for (unsigned t=0;t<50;++t) {
        p.angle=pc_turn(p.angle,cheat_steer(&p,&world,sides,rate),rate);
        pc_collide(&p,world.walls,world.count,sides,world.speed);
        assert(!p.hit);
        pc_world_move(&world);
    }
}
int main(void) {
    PcPlayer p={30,30,0,0};
    pc_world_reset(&world);cheat_reset();
    assert(cheat_steer(&p,&world,6,7)==0);
    for (uint8_t sides=4;sides<=6;++sides) {
        corridor(1,PC_INPUT_POSITIVE,sides,7,22);
        corridor(sides-1,PC_INPUT_NEGATIVE,sides,7,22);
    }
    corridor(1,PC_INPUT_POSITIVE,6,9,40);
    corridor(5,PC_INPUT_NEGATIVE,6,9,40);
    /* Commands must not become physical obstacles. */
    pc_world_reset(&world);cheat_reset();pc_world_add(&world,20,150,400);
    assert(cheat_steer(&p,&world,6,7)==0);
    /* A sealed, already lethal ring remains lethal: no invulnerability. */
    pc_world_reset(&world);cheat_reset();
    for (uint8_t s=0;s<6;++s) pc_world_add(&world,s,145,200);
    p.angle=pc_turn(p.angle,cheat_steer(&p,&world,6,7),7);
    pc_collide(&p,world.walls,world.count,6,world.speed);
    assert(p.hit);
    puts("Cheat assist: both directions, prediction, immutability and normal collision passed");
    return 0;
}
