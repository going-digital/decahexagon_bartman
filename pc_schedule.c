#include "pc_schedule.h"

void pc_schedule_reset(PcSchedule *s) {
    s->wave=0;s->shape_counter=0;s->chosen=-1;
    s->rotation_mode=0;s->rotation_cue=0;s->tilt_request=0;
}

void pc_schedule_tick(PcSchedule *s,PcWorld *w,uint8_t sides,
                      uint32_t score,PcRandom random,void *context) {
    s->chosen=-1;s->rotation_cue=0;s->tilt_request=0;
    if (!w->marker_wait && w->delay_ticks) --w->delay_ticks;
    if (w->delay_ticks) return;
    w->delay_numerator=0;
    uint32_t wave=s->wave;
    if (wave<12) w->speed=22;
    else if (w->speed<(wave<30 ? 25:28)) ++w->speed;
    if (score>3600 && w->speed<33) ++w->speed;
    if (wave==5 || (wave>5 && !(wave&3))) {
        uint8_t old=s->rotation_mode,base=wave==5 ? 0 : (score>3600 ? 4:2);
        do { s->rotation_mode=(uint8_t)(base+random(context,2)); }
        while (s->rotation_mode==old);
        s->rotation_cue=12;
    }
    int16_t chosen=-1;
    if (wave==0) chosen=90;
    else if (wave==10 || (wave>21 && wave%20==0)) {
        s->tilt_request=(uint8_t)(1+random(context,2));
        w->delay_ticks=5;w->delay_numerator=(uint32_t)5*w->speed;
    } else if (!(wave&3) && sides==6) {
        if (!w->morph_state) chosen=101;
    } else if (wave<6) chosen=200;
    else if (wave<12) {
        s->shape_counter=5;
        if (random(context,100)<11) {
            uint16_t r=random(context,3);
            chosen=r==0 ? 103 : r==1 ? 107 : w->speed>=26 ? 108:105;
        } else chosen=200;
    } else if (++s->shape_counter<4) {
        uint8_t effective=(uint8_t)(sides-(w->morph_state==1));
        if (effective>=3 && effective<=5)
            chosen=(int16_t)((random(context,100)<81 ? 300:500)+5-effective);
        else if (effective==6) {
            if (random(context,100)<31) {
                uint16_t r=random(context,3);
                if (r==0) chosen=103;
                else if (r==1) chosen=107;
                else if (w->speed<26) chosen=105;
                else if (score<=3600 || random(context,100)>50) chosen=108;
                else chosen=random(context,100)<51 ? 120:121;
            } else {
                (void)random(context,100); /* Reference consumes an unused draw. */
                chosen=202;
            }
        }
    } else {
        if (sides==3) chosen=405; /* Absent generator case, deliberately no walls. */
        else if (sides==4) chosen=406;
        else if (sides==5) chosen=random(context,100)<51 ? 407 : random(context,100)<51 ? 403:402;
        else if (sides==6) chosen=random(context,100)<51 ? 401:400;
        s->shape_counter=0;
    }
    s->chosen=chosen;
    if (chosen>=0) (void)pc_generate_wave(w,(uint16_t)chosen,random,context);
    ++s->wave;
}
