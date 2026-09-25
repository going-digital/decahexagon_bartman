#include "pc_ending.h"
static int split(PcRandom r,void *c,int a,int b){return r(c,100)<51?a:b;}
int pc_ending_wave(uint32_t ms,uint32_t score,unsigned sides,unsigned shrinking,
                   unsigned speed,PcRandom r,void *c) {
    if(ms>=163500)return -1;
    if(ms>=151300)return 1001;
    if(ms>=140000)return 202;
    if(ms>=96200) {
        unsigned shape=sides-(shrinking==1);
        if(shape>=3 && shape<=5)return r(c,100)<81?305-(int)shape:505-(int)shape;
        if(shape!=6)return -1;
        if(r(c,100)<31) {
            unsigned choice=r(c,3);
            if(choice==0)return 103;
            if(choice==1)return 107;
            if(speed<26)return 105;
            if(score<=3600 || r(c,100)>50)return 108;
            return split(r,c,120,121);
        }
        (void)r(c,100); /* PC consumes this draw even though the result is fixed. */
        return 202;
    }
    if(ms>=68600) {
        static const int ids[]={101,103,104,111};
        unsigned choice=r(c,6);
        return choice<4?ids[choice]:choice==4?split(r,c,113,112):200;
    }
    if(ms>=46500) {
        static const int ids[]={101,102,107,108};
        unsigned choice=r(c,score>3600?6:5);
        return choice<4?ids[choice]:choice==4?split(r,c,110,109):123;
    }
    if(ms<24500)return 200;
    if(r(c,100)<61) {
        if(ms>=35500) {
            if(score<=7200)return 101;
            unsigned choice=r(c,3);
            return choice==0?101:choice==1?102:split(r,c,110,109);
        }
        unsigned choice=r(c,7);
        if(choice<3)return split(r,c,110,109);
        if(choice==3)return 125;
        if(choice==4)return split(r,c,113,112);
        if(choice==5)return split(r,c,600,120);
        return split(r,c,103,123);
    }
    return split(r,c,201,202);
}

int pc_ending_spawn(PcEnding *s,PcWorld *w,uint32_t ms,uint32_t score,
                    unsigned sides,PcRandom random,void *context) {
    unsigned gate=pc_ending_gate(s,w,ms);
    if(gate!=PC_END_GATE_SPAWN)return -1;
    int wave=pc_ending_wave(ms,score,sides,w->morph_state,w->speed,random,context);
    if(wave>=0)(void)pc_generate_wave(w,(uint16_t)wave,random,context);
    return wave;
}
