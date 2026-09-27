#include "pc_morph.h"
void pc_morph_reset(PcMorph *m) { m->sides=6;m->phase=0;m->growing=0; }
void pc_morph_tick(PcMorph *m,PcWorld *w) {
    uint8_t state=w->morph_state;
    switch (state) {
    case 1:
        m->growing=0;
        if (++m->phase>=11) { --m->sides;m->phase=0;w->morph_state=0; }
        break;
    case 2: case 4:
        if (m->sides<6) {
            ++m->sides;m->phase=11;m->growing=1;
            w->morph_state=(uint8_t)(w->morph_state+1);
        } else w->morph_state=0;
        break;
    case 3: case 5:
        if (m->phase) --m->phase;
        if (!m->phase) w->morph_state=0;
        break;
    }
    /* Source refreshes this+0x54c0 to 20 while processing states 1/2/3 (the
     * ordinary marker-driven shrink/grow, not the death regrowth 4/5), and
     * otherwise lets it decay; pc_world_move stalls while it is nonzero. */
    if (state>=1 && state<=3) w->freeze=20;
    else if (w->freeze) --w->freeze;
}
/* int(360/(sides-fraction)), generated using repeated binary64 +/-0.1.
 * Keeping the two trajectories separate retains the tiny positive growth
 * residue and the just-below-one shrink endpoint of the desktop routine. */
#include "pc_morph_data.inc"
uint16_t pc_morph_arc(const PcMorph *m) {
    return arcs[m->growing][m->sides-3][m->phase];
}
