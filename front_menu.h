#pragma once
#include "pc_sfx.h"
/* Menu pages stay inside MODE_ATTRACT, preserving resident audio/save policy. */
enum { FRONT_HOME, FRONT_OPTIONS, FRONT_CREDITS, FRONT_LEVELS, FRONT_NAME, FRONT_WRITE_PROTECT };
#define FRONT_CREDIT_PAGES 7
struct FrontMenu { unsigned char page, choice, credits, held, level; short slide; };
#ifdef __m68k__
__attribute__((optimize("Os")))
#endif
static inline uint16_t front_menu_tick(struct FrontMenu *m,unsigned held,unsigned confirm,unsigned back) {
    if(m->slide) {int d=(m->slide>0?m->slide:-m->slide);d=(d+3)/4;
        m->slide+=m->slide>0?-d:d;}
    unsigned edge=held & ~m->held;
    m->held=held;
    if(back) {unsigned changed=m->page!=FRONT_HOME;m->page=FRONT_HOME;m->slide=0;return changed?SFX_BIT(SFX_RANKUP):0;}

    uint16_t sound=0;
    /* Gameplay positive means left; menu right advances the selection. */
    int step=(edge&2)?1:(edge&1)?-1:0;
    if(m->page==FRONT_LEVELS) {
        if(step && !m->slide) {m->slide=step*192;m->level=(m->level+6+step)%6;
            sound=SFX_BIT(SFX_MENUCHOOSE);}
    } else if(m->page==FRONT_HOME) {
        if(step && !m->slide) {m->slide=step*192;m->choice=(m->choice+3+step)%3;sound=SFX_BIT(SFX_MENUCHOOSE);}
        if(confirm) {
            m->page=m->choice==0?FRONT_LEVELS:m->choice==1?FRONT_OPTIONS:FRONT_CREDITS;
            m->credits=0;
            sound=SFX_BIT(SFX_MENUSELECT);
        }
    } else if(m->page==FRONT_CREDITS) {
        if(confirm)step=1;
        if(step) {m->credits=(m->credits+FRONT_CREDIT_PAGES+step)%FRONT_CREDIT_PAGES;
            sound=SFX_BIT(confirm?SFX_MENUSELECT:SFX_MENUCHOOSE);}
    }
    return sound;
}
