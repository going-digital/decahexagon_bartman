#include "pc_death.h"
void pc_death_tick(PcLifecycle *life,PcWorld *w,PcMorph *m,uint8_t active_stage,uint8_t selected_stage) {
    /* PC clears continuation flags when its extent would exceed 320. */
    uint8_t stage=life->extent==320 ? selected_stage:active_stage;
    pc_morph_tick(m,w);
    pc_lifecycle_tick(life);
    /* PC's death velocity is zero until timer 60, then -40. The special
     * movement branch multiplies by five and touches inactive records too.
     * It does not consume command markers or release their scheduler wait. */
    for (uint16_t i=0;i<w->count;++i) {
        PcWall *wall=&w->walls[i];
        if (life->death>=60) wall->distance+=200;
        if (wall->distance<1) wall->distance=0;
    }
    while (w->count && !w->walls[w->count-1].active) --w->count;
    /* Hexagon restores missing sides only once the result extent is open.
     * This request follows the effects update, so growth begins next tick. */
    if (stage==0 && life->extent>=320 && m->sides<6 && !w->morph_state)
        w->morph_state=4;
}
