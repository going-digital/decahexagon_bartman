#include "../pc_ending_polygon.h"
#include <assert.h>
#include <stdio.h>
int main(void) {
    PcEndingView view={0,0,600,160,100,600,600};
    PcEndingVertex quad[4]={{-100,-50,0},{100,-50,0},{100,50,0},{-100,50,0}};
    PcEndingPoint out[8];
    assert(pc_ending_polygon(&view,quad,319,199,out)==4);
    assert(out[0].x==60 && out[0].y==50);
    assert(out[2].x==260 && out[2].y==150);
    view.depth=0;
    assert(pc_ending_polygon(&view,quad,319,199,out)==4);
    for(unsigned i=0;i<4;i++) {
        assert(out[i].x==0 || out[i].x==319);
        assert(out[i].y==0 || out[i].y==199);
    }
    out[0]=(PcEndingPoint){123,456};quad[3].x=8193;
    assert(pc_ending_polygon(&view,quad,319,199,out)==-1);
    assert(out[0].x==123 && out[0].y==456);
    quad[3].x=-100;
    PcEndingVertex temp=quad[1];quad[1]=quad[2];quad[2]=temp;
    assert(pc_ending_polygon(&view,quad,319,199,out)==-1);
    assert(out[0].x==123 && out[0].y==456);
    for(unsigned i=0;i<4;i++)quad[i]=(PcEndingVertex){0,0,0};
    assert(pc_ending_polygon(&view,quad,319,199,out)==0);
    assert(out[0].x==123 && out[0].y==456);
    puts("Ending polygon: visible, enclosing, invalid, crossed and degenerate quads pass");
}
