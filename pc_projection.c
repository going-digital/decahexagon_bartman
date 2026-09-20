#include "pc_projection.h"

static int32_t project_div5(int32_t value) {
#ifdef __m68k__
    /* DIVS.W takes a 32-bit dividend but returns a signed 16-bit quotient.
     * Use an unsigned range check so even INT32_MIN/MAX are safe. Keep the
     * general division for unusually large world/death coordinates. */
    if ((uint32_t)value + 163840u < 327680u) {
        __asm__("divs.w %1,%0" : "+d"(value) : "d"((int16_t)5) : "cc");
        return (int16_t)value; /* discard remainder and sign-extend quotient */
    }
#endif
    return value / 5;
}

void pc_span_shared_edges(const PcSpan *spans,uint16_t count,uint8_t sides,uint8_t *shared) {
    uint16_t start[7],cursor=0;
    for (uint16_t i=0;i<count;++i) shared[i]=0;
    for (uint8_t slot=0;slot<sides;++slot) {
        start[slot]=cursor;
        while (cursor<count && spans[cursor].slot==slot) ++cursor;
    }
    start[sides]=count;
    /* Merge adjacent slot lists. Each span is visited at most twice, and
     * both edge flags are set together, including the last/first seam. */
    for (uint8_t slot=0;slot<sides;++slot) {
        uint8_t next=slot+1==sides?0:slot+1;
        uint16_t i=start[slot],j=start[next];
        while (i<start[slot+1] && j<start[next+1]) {
            if (spans[i].inner<spans[j].inner) ++i;
            else if (spans[j].inner<spans[i].inner) ++j;
            else {
                if (spans[i].outer==spans[j].outer) {
                    shared[i]|=PC_SPAN_NEXT;
                    shared[j]|=PC_SPAN_PREV;
                }
                ++i;++j;
            }
        }
    }
}

uint16_t pc_project_spans(const PcWorld *world,uint8_t sides,PcSpan *spans) {
    uint16_t count=0;
    for (uint16_t i=0;i<world->count;++i) {
        const PcWall *w=&world->walls[i];
        if (!w->active || w->slot>=sides || w->width<5) continue;
        PcSpan p;
        p.slot=w->slot;
        /* Desktop truncates distance and width separately before addition. */
        p.inner=(int16_t)(40+project_div5(w->distance));
        p.outer=(int16_t)(p.inner+project_div5(w->width));
        uint16_t j=count;
        while (j && (spans[j-1].slot>p.slot ||
               (spans[j-1].slot==p.slot && spans[j-1].inner>p.inner))) {
            spans[j]=spans[j-1];--j;
        }
        spans[j]=p;++count;
    }
    uint16_t merged=0;
    for (uint16_t i=0;i<count;++i) {
        PcSpan p=spans[i];
        if (merged && spans[merged-1].slot==p.slot && spans[merged-1].outer>=p.inner) {
            if (p.outer>spans[merged-1].outer) spans[merged-1].outer=p.outer;
        } else spans[merged++]=p;
    }
    return merged;
}
