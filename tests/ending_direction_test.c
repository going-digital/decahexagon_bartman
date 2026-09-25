#include <assert.h>
#include <string.h>
#include <stdio.h>
#include "pc_projection.h"
int main(void) {
    PcWorld w; PcSpan s[PC_WALL_CAPACITY];
    pc_world_reset(&w); w.speed=20;
    pc_world_add(&w,0,4000,200);
    assert(pc_project_ending_spans(&w,6,s)==0);
    pc_world_move(&w);
    assert(pc_project_ending_spans(&w,6,s)==1);
    assert(s[0].inner==40 && s[0].outer==44);
    for(unsigned i=0;i<19;++i)pc_world_move(&w);
    PcWorld before=w;
    assert(pc_project_ending_spans(&w,6,s)==1);
    assert(s[0].inner==80 && s[0].outer==120);
    assert(!memcmp(&w,&before,sizeof(w)));
    assert(pc_project_spans(&w,6,s)==1);
    assert(s[0].inner==760 && s[0].outer==800);
    pc_world_add(&w,20,20,0);w.marker_wait=1;
    pc_world_move(&w);
    assert(w.morph_state==1 && !w.marker_wait);
    while(w.count)pc_world_move(&w);
    assert(!pc_project_ending_spans(&w,6,s));
    puts("Ending direction: outward emergence, unchanged gameplay, marker timing and retirement pass");
}
