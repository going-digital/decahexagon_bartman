#include "../pc_ending.h"
#include <assert.h>
#include <stdio.h>
static unsigned draws,seed;
static uint16_t random_value(void *unused,uint16_t bound) {
    (void)unused;
    uint32_t x=seed+(++draws)*0x9e3779b9u;
    x^=x>>16;x*=0x7feb352du;x^=x>>15;
    return x%bound;
}
int main(int argc,char **argv) {
    assert(argc==2);FILE *f=fopen(argv[1],"r");assert(f);
    unsigned ms,score,sides,shrinking,speed,expected_draws,n=0;int expected;
    while(fscanf(f,"%u %u %u %u %u %u %d %u",&ms,&seed,&score,&sides,&shrinking,&speed,&expected,&expected_draws)==8) {
        draws=0;
        int chosen=pc_ending_wave(ms,score,sides,shrinking,speed,random_value,0);
        if(chosen!=expected || draws!=expected_draws){
            fprintf(stderr,"ms=%u seed=%u: got %d/%u expected %d/%u\n",ms,seed,chosen,draws,expected,expected_draws);return 1;
        }
        n++;
    }
    fclose(f);assert(n==27200);
    /* Exercise the actual generator adapter as well as isolated choices. */
    PcEnding ending;pc_ending_init(&ending);ending.phase=2;
    PcWorld world;pc_world_reset(&world);world.speed=22;
    draws=seed=0;
    assert(pc_ending_spawn(&ending,&world,0,7202,6,random_value,0)==200);
    assert(world.delay_ticks>1 && !world.overflow);
    unsigned before=draws,delay=world.delay_ticks;
    assert(pc_ending_spawn(&ending,&world,16,7202,6,random_value,0)==-1);
    assert(draws==before && world.delay_ticks==delay-1);
    pc_world_reset(&world);world.speed=40;ending.phase=14;
    assert(pc_ending_spawn(&ending,&world,151300,7202,6,random_value,0)==1001);
    assert(world.count && !world.overflow);
    before=draws;world.delay_ticks=0;
    assert(pc_ending_spawn(&ending,&world,168000,7202,6,random_value,0)==-1);
    assert(ending.phase==255 && draws==before);
    printf("Ending selector: %u native PC wave choices and RNG counts match\n",n);
}
