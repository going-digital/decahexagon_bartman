#include <stdio.h>
#include <stdlib.h>
#include "../pc_world.h"
static PcWorld world;
static unsigned rolls[128],cursor,count;
static uint16_t random_draw(void *unused,uint16_t bound) {
    (void)unused;
    if (cursor>=count || rolls[cursor]>=bound) { fprintf(stderr,"RNG mismatch\n");exit(2); }
    return (uint16_t)rolls[cursor++];
}
int main(void) {
    unsigned wave,speed,id=0;
    while (scanf("%u %u %u",&wave,&speed,&count)==3) {
        if (count>128) return 2;
        for (unsigned i=0;i<count;++i) if(scanf("%u",&rolls[i])!=1)return 2;
        cursor=0;pc_world_reset(&world);world.speed=(uint16_t)speed;
        if(!pc_generate_wave(&world,(uint16_t)wave,random_draw,0))return 3;
        printf("%u %u %u %.9g %u %d %u",wave,id++,world.count,
            (double)((float)world.delay_numerator/world.speed),world.marker_wait,world.spawn_base,cursor);
        for(unsigned i=0;i<world.count;++i)printf(" %u,%d,%d,0,%u",world.walls[i].slot,
            (int)world.walls[i].distance,(int)world.walls[i].width,world.walls[i].active);
        printf("\n");
    }
    return 0;
}
