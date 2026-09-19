#include "pc_progress.h"

static void rotation(PcSchedule *s,uint8_t base,uint8_t old,
                     PcRandom random,void *context) {
    do { s->rotation_mode=(uint8_t)(base+random(context,2)); }
    while (s->rotation_mode==old);
}
void pc_progress_reset(PcProgress *p,PcWorld *w,uint8_t stage,uint8_t hyper,
                       uint8_t old_rotation,PcRandom random,void *context) {
    pc_schedule_reset(&p->schedule);pc_world_reset(w);
    p->selected_stage=stage;p->hyper=hyper;p->handoffs=p->transitioned=0;
    p->schedule.stage=stage;
    w->speed=hyper ? (stage==2 ? 40:33):(stage==2 ? 35:stage==1 ? 24:22);
    if (hyper) { p->schedule.wave=stage ? 51:31;p->schedule.hyper_entry=1; }
    rotation(&p->schedule,hyper ? 4:stage==2 ? 2:0,old_rotation,random,context);
}
uint32_t pc_progress_score(const PcProgress *p,uint32_t elapsed) {
    return elapsed+(p->hyper ? 3600u:0u)-(uint32_t)p->handoffs*7200u;
}
void pc_progress_tick(PcProgress *p,PcWorld *w,uint8_t sides,uint32_t elapsed,
                      PcRandom random,void *context) {
    PcSchedule *s=&p->schedule;
    uint32_t score=pc_progress_score(p,elapsed);
    p->transitioned=0;
    if (s->stage<2 && score>10800) {
        uint8_t old=s->rotation_mode;
        /* changetostage clears walls, waits, morph and delay, but preserves
         * the player's angle and total run score. It sets wave51 even for
         * the outgoing Hexagon selector. See native boundary fixtures. */
        pc_world_reset(w);w->speed=s->stage==0 ? 33:40;
        s->wave=51;s->hyper_entry=1;
        rotation(s,4,old,random,context);
        sides=6;
        p->transitioned=1;
    }
    /* The outgoing selector emits 93/94 on the handoff tick. Switching the
     * selector first would wrongly emit 94/95 and change RNG/geometry. */
    pc_schedule_tick(s,w,sides,score,random,context);
    if (p->transitioned) { ++s->stage;++p->handoffs;s->late_phase=0; }
}
