#include <stdio.h>
#include "../pc_schedule.h"
static PcWorld world;
static uint32_t seed,draws;
static uint16_t draw(void *unused,uint16_t bound) {
    (void)unused;
    uint32_t x=seed+(++draws)*0x9e3779b9u;
    x^=x>>16;x*=0x7feb352du;x^=x>>15;
    return (uint16_t)(x%bound);
}
int main(void) {
    unsigned wave,sides,score,shape,input_seed;
    while (scanf("%u %u %u %u %u",&wave,&sides,&score,&shape,&input_seed)==5) {
        PcSchedule s;pc_schedule_reset(&s);pc_world_reset(&world);
        s.wave=wave;s.shape_counter=(uint16_t)shape;seed=input_seed;draws=0;
        pc_schedule_tick(&s,&world,(uint8_t)sides,score+1,draw,0);
        if (s.chosen>=0) printf("%d",s.chosen);
        printf(",%u,%u,%u,%.9g,%u\n",(unsigned)s.wave,s.shape_counter,world.speed,
               (double)((float)world.delay_numerator/world.speed),world.marker_wait);
    }
    return 0;
}
