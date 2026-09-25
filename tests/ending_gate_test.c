#include "../pc_ending.h"
#include <assert.h>
#include <stdio.h>
int main(int argc,char **argv) {
    assert(argc==2);FILE *f=fopen(argv[1],"r");assert(f);
    unsigned ms,marker,delay,expected_delay,stage,count=0;int wave;
    while(fscanf(f,"%u %u %u %u %u %d",&ms,&marker,&delay,&expected_delay,&stage,&wave)==6) {
        PcEnding s;pc_ending_init(&s);s.phase=15;
        PcWorld w={0};w.speed=22;w.delay_ticks=delay;w.marker_wait=marker;
        unsigned result=pc_ending_gate(&s,&w,ms);
        assert(w.delay_ticks==expected_delay);
        assert((s.phase==255)==(stage==2));
        assert((result==PC_END_GATE_SPAWN)==(wave>=0));
        if(result==PC_END_GATE_FINISH)assert(pc_ending_gate(&s,&w,ms+1)==PC_END_GATE_WAIT);
        count++;
    }
    fclose(f);assert(count==56);
    puts("Ending gate: all 56 native delay, marker and final-exit cases match");
}
