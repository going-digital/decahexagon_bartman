#include "pc_ending_flip.h"
void pc_ending_flip_request(PcEndingFlip *flip) { flip->phase=1; }
int16_t pc_ending_flip_tick(PcEndingFlip *s,unsigned mode) {
    int16_t delta=0;
    switch(s->phase) {
    case 1:
        s->phase=(mode==1 || mode==3 || mode==5 || mode==7) ? 2:5;
        break;
    case 2: case 5:
        ++s->timer;
        delta=s->phase==2 ? s->timer:-(int16_t)s->timer;
        if(s->timer>=14)s->phase=s->phase==2 ? 3:6;
        break;
    case 3: case 6:
        ++s->timer;delta=s->phase==3 ? 14:-14;
        if(s->timer>=45){s->timer=10;s->phase=s->phase==3 ? 4:7;}
        break;
    case 4: case 7:
        if(s->timer)--s->timer;
        delta=s->phase==4 ? s->timer:-(int16_t)s->timer;
        if(!s->timer)s->phase=0;
        break;
    default: break;
    }
    return delta;
}
