#include "pc_menu.h"
void pc_menu_reset(PcMenu *m,uint8_t profile) {
    m->wedge=profile ? 6-profile:0;
    m->angle=m->wedge*60+30;m->motion=0;m->cooldown=0;
}
uint8_t pc_menu_profile(const PcMenu *m) { return m->wedge ? 6-m->wedge:0; }
void pc_menu_tick(PcMenu *m,uint8_t held) {
    if (m->cooldown) --m->cooldown;
    else if (!m->motion) {
        if (held&PC_INPUT_POSITIVE) m->motion=-10;
        else if (held&PC_INPUT_NEGATIVE) m->motion=10;
        if (m->motion) m->cooldown=8;
    }
    if (m->motion<0) {
        m->angle+=6;
        if (++m->motion==0) { m->wedge=(m->wedge+1)%6;m->angle=m->wedge*60+30; }
    } else if (m->motion>0) {
        m->angle-=6;
        if (--m->motion==0) { m->wedge=(m->wedge+5)%6;m->angle=m->wedge*60+30; }
    }
    if (m->angle<0) m->angle+=360;
    if (m->angle>=360) m->angle-=360;
}
uint8_t pc_profile_unlocked(const PcRecords *r,uint8_t profile) {
    return profile<3 || r->completed[profile-3];
}
uint8_t pc_record_tick(PcRecords *r,uint8_t profile,uint32_t elapsed) {
    uint8_t improved=elapsed>r->best[profile];
    if (improved) r->best[profile]=elapsed;
    /* PC winlevel is dispatched strictly after the 3600-tick threshold,
     * using the selected profile and displayed score, before continuation. */
    if (elapsed>3600) r->completed[profile]=1;
    return improved;
}
