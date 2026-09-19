#include "pc_projection.h"
uint16_t pc_project_spans(const PcWorld *world,uint8_t sides,PcSpan *spans) {
    uint16_t count=0;
    for (uint16_t i=0;i<world->count;++i) {
        const PcWall *w=&world->walls[i];
        if (!w->active || w->slot>=sides || w->width<5) continue;
        PcSpan p;
        p.slot=w->slot;
        /* Desktop truncates distance and width separately before addition. */
        p.inner=(int16_t)(40+w->distance/5);
        p.outer=(int16_t)(p.inner+w->width/5);
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
