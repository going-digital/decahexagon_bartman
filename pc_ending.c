#include "pc_ending.h"
void pc_ending_init(PcEnding *s) {
    *s=(PcEnding){0};s->palette=30;s->wave_mode=8;
}
static unsigned transition(PcEnding *s,unsigned phase,unsigned speed,
                           unsigned wave,unsigned palette) {
    s->phase=phase;s->speed=speed;s->wave_mode=wave;s->palette=palette;
    return PC_END_PALETTE|PC_END_FLIP;
}
unsigned pc_ending_step(PcEnding *s,uint32_t ms,unsigned flip,unsigned blending) {
    unsigned events=0;
    /* Spawn/exit gating follows phase updates in pc_ending_gate. */
    if(s->phase==255)return 0;
    if(s->wait) --s->wait;
    else switch(s->phase) {
    case 0: s->rotation=0;s->speed=0;s->phase=1;break;
    case 1:
        s->rotation=0;
        if(++s->speed<=5)s->wait=5;
        else {s->speed=5;s->phase=2;}
        break;
    case 2:
        s->rotation=0;
        if(ms>=24500)events=transition(s,3,20,1,9);
        break;
    case 3: s->rotation=0;if(!flip)s->phase=4;break;
    case 4:
        s->rotation=0;
        if(ms>=35500)events=transition(s,5,35,2,10);
        break;
    case 5:
        if(ms>=46500)events=transition(s,6,35,3,7);
        else if(ms>=37000 && !blending) {
            static const uint8_t colours[]={11,12,13,14,15,10,11,12,13,14};
            unsigned i=(ms-37000)/1000;
            if(i>9)i=9;
            s->palette=colours[i];events=PC_END_PALETTE;
        }
        break;
    case 6:
        s->rotation=1;s->camera_target=20;
        if(ms>=57600)events=transition(s,7,45,4,6);
        break;
    case 7:
        s->rotation=1;s->camera_target=20;
        if(ms>=68600)events=transition(s,8,45,3,5);
        break;
    case 8:
        s->rotation=1;s->camera_target=20;
        if(ms>=96200){events=transition(s,9,45,5,1);s->hyper=1;}
        break;
    case 9:
        s->rotation=0;s->camera_target=30;
        if(ms>=107500){events=transition(s,10,45,6,3);s->scene_mode=1;}
        break;
    case 10: if(ms>=114000){s->scene_mode=1;s->phase=11;}break;
    case 11:
        s->camera_target=30;
        if(ms>=118000){events=transition(s,101,30,5,0)|PC_END_RANDOM_ROTATION;s->scene_mode=2;}
        break;
    case 101: if(ms>=129500){s->scene_mode=2;s->phase=12;}break;
    case 12:
        s->camera_target=30;
        if(ms>=140000)events=transition(s,13,20,2,200);
        break;
    case 13:
        s->camera_target=0;
        if(ms>=151300)events=transition(s,14,40,8,30);
        break;
    case 14: if(ms>=163500){s->phase=15;events=PC_END_STOP_MUSIC;}break;
    }
    return events;
}
unsigned pc_ending_tick(PcEnding *s,unsigned flip,unsigned blending) {
    if(s->phase==255)return 0;
    ++s->ticks;
    return pc_ending_step(s,s->ticks/60*1000+s->ticks%60*1000/60,flip,blending);
}

unsigned pc_ending_gate(PcEnding *s,PcWorld *w,uint32_t ms) {
    if(s->phase==255)return PC_END_GATE_WAIT;
    if(!w->marker_wait && w->delay_ticks)--w->delay_ticks;
    if(w->delay_ticks)return PC_END_GATE_WAIT;
    w->delay_numerator=0;
    if(ms>=163500) {
        w->delay_ticks=30;w->delay_numerator=30u*w->speed;
        if(ms>=168000){s->phase=255;return PC_END_GATE_FINISH;}
        return PC_END_GATE_WAIT;
    }
    return PC_END_GATE_SPAWN;
}

unsigned pc_ending_death_entry(unsigned completion,unsigned death_ticks,
                               unsigned extent,unsigned suppressed) {
    return completion==3 && death_ticks>=60 && extent>199 && !suppressed;
}
