#include <assert.h>
#include "../pc_pulse.h"
int main(void) {
    /* Independently transcribed PC float dt=1 envelope, for every source cue,
     * stage divisor and reachable previous envelope. */
    for(unsigned stage=0;stage<3;++stage)
        for(int cue=-120;cue<=120;++cue)
            for(unsigned old=0;old<=80;++old) {
                double x=(double)cue/(stage==2?3:2);
                if(x<0) x=-x;
                double target=(int)x,env=old;
                if(env<target) env=target;
                else {env-=1;if(env<0) env=0;}
                if(env>0) env-=1;
                assert(pc_pulse_tick(old,cue,stage)==(unsigned)env);
            }
    assert(pc_pulse_cue_index(611)==0);
    assert(pc_pulse_cue_index(811)==0);
    assert(pc_pulse_cue_index(812)==1);
    assert(pc_pulse_cue_index(120000)==596);
    /* Audible cursor follows the DMA buffer, not the producer's 4-buffer lead.
     * A repeated buffer uses the same start, and a loop wraps within a block. */
    assert(pc_pcm_position(1024,2320000,0,296)==1024);
    assert(pc_pcm_position(1024,2320000,312,296)==1263);
    assert(pc_pcm_position(1024,2320000,262,298)==1223);
    assert(pc_pcm_position(1024,2320000,1000,296)==1535);
    assert(pc_pcm_position(2319900,2320000,312,296)==139);
    assert(pc_pcm_position(0,2320000,0,296)==0); /* retry */
    return 0;
}
