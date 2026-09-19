#include <stdio.h>
#include "../pc_schedule.h"
#include "../pc_morph.h"
static PcWorld world;
static uint32_t rng_state;
static uint16_t draw(void *unused,uint16_t bound) {
    (void)unused;
    rng_state^=rng_state<<13;rng_state^=rng_state>>17;rng_state^=rng_state<<5;
    return (uint16_t)(rng_state%bound);
}
int main(void) {
    for (unsigned stage=0;stage<2;++stage) {
    unsigned peak=0,morphs=0,side_mask=0;
    /* Force survival to exercise scheduling/storage, not an input replay or
     * a claim of reference-equivalent complete runs. Stop at stage boundary. */
    for (unsigned seed=1;seed<=256;++seed) {
        PcSchedule s;PcMorph m;
        pc_world_reset(&world);pc_schedule_reset(&s);pc_morph_reset(&m);s.stage=(uint8_t)stage;rng_state=seed;
        for (unsigned tick=1;tick<=10800;++tick) {
            pc_morph_tick(&m,&world);
            if (m.sides<3 || m.sides>6 || m.phase>11) return 1;
            side_mask|=1u<<m.sides;
            unsigned before=world.morph_state;
            pc_world_move(&world);
            if (before!=world.morph_state) {
                if (before || m.phase) {fprintf(stderr,"Interrupted morph seed=%u tick=%u\n",seed,tick);return 2;}
                ++morphs;
            }
            pc_schedule_tick(&s,&world,m.sides,tick,draw,0);
            if (world.overflow) return 3;
            if (world.count>peak) peak=world.count;
        }
    }
    if (stage==0 ? (side_mask!=0x70 || !morphs) : (side_mask!=0x40 || morphs)) return 4;
    printf("Stage%u: 256 forced-survival runs through tick10800: peak %u records, %u uninterrupted morphs\n",stage,peak,morphs);
    }
    return 0;
}
