#include "pc_schedule.h"

void pc_schedule_reset(PcSchedule *s) {
    s->stage=0;s->wave=0;s->shape_counter=0;s->chosen=-1;
    s->late_phase=0;s->flip_request=0;s->rotation_mode=0;s->rotation_cue=0;s->tilt_request=0;
}

/* These draws intentionally retain reference branches that emit no wave.
 * Selection is independent of polygon count in stage1. */
static int16_t split(PcRandom random,void *context,int16_t a,int16_t b) {
    return random(context,100)<51 ? a:b;
}
static int16_t hexagoner_early(PcRandom random,void *context) {
    static const int16_t ids[]={101,103,104,111};
    uint16_t r=random(context,5);
    return r<4 ? ids[r]:split(random,context,113,112);
}
static int16_t hexagoner_wave(PcSchedule *s,PcWorld *w,uint32_t score,
                             PcRandom random,void *context) {
    uint32_t wave=s->wave;
    if (!wave) return 91;
    if (wave%20==0) {
        static const int16_t ids[]={101,103,108};
        w->camera_trigger=1;
        uint16_t r=random(context,4);
        return r<3 ? ids[r]:split(random,context,113,112);
    }
    if (!(wave&3)) return 101;
    if (wave<6) return wave==1 ? hexagoner_early(random,context):200;
    if (wave<12) return random(context,100)<=20 ? hexagoner_early(random,context):200;
    if (wave<25) {
        if (random(context,100)<=30) {
            static const int16_t ids[]={101,103,104,108};
            uint16_t r=random(context,6);
            if (r<4) return ids[r];
            if (r==4) return split(random,context,113,112);
            return -1; /* Reference's sixth branch makes no generatewave call. */
        }
        return split(random,context,200,201);
    }
    if (wave<50) {
        static const int16_t ids[]={101,102,104,108};
        return random(context,100)<41 ? ids[random(context,4)]:201;
    }
    if (random(context,100)<=60) {
        static const int16_t ids[]={101,102,107,108};
        uint16_t r=random(context,score>3600 ? 6:5);
        if (r<4) return ids[r];
        if (r==4) return split(random,context,110,109);
        return 123;
    }
    return split(random,context,201,202);
}

/* Normal stage 2 only; the wave95 hyper-entry override belongs to progression. */
static int16_t hexagonest_wave(PcSchedule *s,PcWorld *w,uint32_t score,
                              PcRandom random,void *context) {
    static const int16_t early[]={101,102,104,108};
    uint32_t wave=s->wave;
    if (!wave) return 92;
    if (wave%20==0) {
        w->camera_trigger=1;
        return early[random(context,4)];
    }
    if (!(wave&3)) return 101;
    if (wave<6) return 200;
    if (wave<12) return random(context,100)<=20 ? early[random(context,4)]:200;
    if (wave<25) {
        if (random(context,100)<=30) {
            uint16_t r=random(context,5);
            return r<4 ? early[r]:split(random,context,110,109);
        }
        return split(random,context,200,201);
    }
    if (wave<50) {
        if (random(context,100)<41) {
            uint16_t r=random(context,4);
            return r<2 ? early[r]:r==2 ? 108:split(random,context,110,109);
        }
        return 201;
    }
    if (random(context,100)<=60) {
        switch (random(context,score>7200 ? 7:score>3600 ? 4:3)) {
        case 0:return 101;
        case 1:return 102;
        case 2:return split(random,context,110,109);
        case 3:return 125;
        case 4:return split(random,context,113,112);
        case 5:return split(random,context,600,120);
        default:return split(random,context,103,123);
        }
    }
    return split(random,context,201,202);
}

void pc_schedule_tick(PcSchedule *s,PcWorld *w,uint8_t sides,
                      uint32_t score,PcRandom random,void *context) {
    s->chosen=-1;s->rotation_cue=0;s->tilt_request=0;s->flip_request=0;
    if (s->stage==2 && score>7200 && !s->late_phase) {
        s->late_phase=1;
        s->rotation_mode=(uint8_t)(8+random(context,2));
        w->camera_trigger=1;
        /* clearenemies preserves the running wave delay and speed. */
        for (uint16_t i=0;i<w->count;++i) w->walls[i].active=0;
        w->count=0;
    }
    if ((s->stage!=0 || !w->marker_wait) && w->delay_ticks) --w->delay_ticks;
    if (w->delay_ticks) return;
    w->delay_numerator=0;
    uint32_t wave=s->wave;
    if (s->stage==2) {
        if (wave<12) w->speed=35;
        if (score>3600) w->speed=40;
    } else if (s->stage==1) {
        if (wave<12) w->speed=24;
        else if (wave<30 && w->speed<28) ++w->speed;
    } else {
        if (wave<12) w->speed=22;
        else if (w->speed<(wave<30 ? 25:28)) ++w->speed;
    }
    if (s->stage!=2 && score>3600 && w->speed<33) ++w->speed;
    if (wave==5 || (wave>5 && !(wave&3))) {
        uint8_t old=s->rotation_mode,base=wave==5 ? 0 : (score>3600 ? 4:2);
        if (s->stage==2) base=wave==5 ? 2:score>7200 ? 8:score>3600 ? 6:4;
        do { s->rotation_mode=(uint8_t)(base+random(context,2)); }
        while (s->rotation_mode==old);
        s->rotation_cue=12;
    }
    int16_t chosen=-1;
    if (s->stage==2) {
        if (wave==12 || wave==24 || wave==48 || (score>8000 && wave%5==0)) s->flip_request=1;
        chosen=hexagonest_wave(s,w,score,random,context);
    }
    else if (s->stage==1) chosen=hexagoner_wave(s,w,score,random,context);
    else if (wave==0) chosen=90;
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
