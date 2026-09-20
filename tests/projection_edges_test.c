#include "../pc_projection.h"
#include <assert.h>
#include <stdio.h>

static PcWorld world;
static PcSpan spans[PC_WALL_CAPACITY];
static uint8_t shared[PC_WALL_CAPACITY];
static uint32_t random_state=17;
static uint32_t random_value(void) {
    random_state=random_state*1664525u+1013904223u;
    return random_state;
}
static void check(unsigned sides) {
    unsigned count=pc_project_spans(&world,sides,spans);
    pc_span_shared_edges(spans,count,sides,shared);
    for (unsigned i=0;i<count;++i) {
        unsigned expected=0;
        unsigned prev=spans[i].slot?spans[i].slot-1:sides-1;
        unsigned next=(spans[i].slot+1)%sides;
        for (unsigned j=0;j<count;++j) {
            if (spans[j].inner!=spans[i].inner || spans[j].outer!=spans[i].outer) continue;
            if (spans[j].slot==prev) expected|=PC_SPAN_PREV;
            if (spans[j].slot==next) expected|=PC_SPAN_NEXT;
        }
        assert(shared[i]==expected);
    }
}
int main(void) {
    for (unsigned sides=3;sides<=6;++sides) {
        world.count=0;check(sides);
        /* A complete ring exercises both neighbours and the wrap seam. */
        world.count=sides;
        for (unsigned i=0;i<sides;++i)
            world.walls[i]=(PcWall){100,50,i,1};
        check(sides);
        for (unsigned i=0;i<sides;++i)
            assert(shared[i]==(PC_SPAN_PREV|PC_SPAN_NEXT));
        world.walls[0].width=75;check(sides);
        world.walls[1].active=0;check(sides);
        for (unsigned trial=0;trial<2000;++trial) {
            world.count=random_value()%(PC_WALL_CAPACITY+1);
            for (unsigned i=0;i<world.count;++i) {
                PcWall *w=&world.walls[i];
                w->slot=(random_value()>>16)%(sides+1);
                w->distance=((random_value()>>16)%120)*25;
                w->width=((random_value()>>16)%8)*25;
                w->active=(random_value()>>16)%4!=0;
            }
            check(sides);
        }
    }
    puts("Projection edge flags match exhaustive neighbour search (8,000 worlds).");
}
