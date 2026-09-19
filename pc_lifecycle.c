#include "pc_lifecycle.h"
void pc_lifecycle_init(PcLifecycle *p) { p->extent=320;p->death=0;p->elapsed=0; }
void pc_lifecycle_start(PcLifecycle *p,PcPlayer *player) {
    p->death=0;p->elapsed=0;player->hit=player->blocked=0;
    /* restart clears the hit state, but preserves both angle fields. */
}
uint8_t pc_lifecycle_can_start(const PcLifecycle *p) { return p->extent==320; }
void pc_lifecycle_die(PcLifecycle *p) { p->death=1; }
void pc_lifecycle_tick(PcLifecycle *p) {
    if (!p->death) {
        ++p->elapsed;
        if (p->extent>40) p->extent-=20;
    } else {
        if (p->death<100) ++p->death;
        if (p->death>=60 && p->extent<320) p->extent+=20;
    }
}
