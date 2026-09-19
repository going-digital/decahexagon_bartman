#include "pc_world.h"

void pc_world_reset(PcWorld *w) {
    for (uint16_t i=0;i<PC_WALL_CAPACITY;++i) w->walls[i].active=0;
    w->count=0; w->speed=22; w->delay_ticks=0; w->delay_numerator=0;
    w->spawn_base=0; w->marker_wait=0; w->morph_state=0;
    w->camera_trigger=0; w->overflow=0;
}

int pc_world_add(PcWorld *w, uint8_t slot, int32_t distance, int32_t width) {
    uint16_t i=0;
    while (i<w->count && w->walls[i].active) ++i;
    if (i==PC_WALL_CAPACITY) { w->overflow=1; return 0; }
    if (i==w->count) ++w->count;
    w->walls[i].slot=slot; w->walls[i].distance=distance;
    w->walls[i].width=width; w->walls[i].active=1;
    return 1;
}

void pc_world_move(PcWorld *w) {
    for (uint16_t i=0;i<w->count;++i) {
        PcWall *wall=&w->walls[i];
        if (!wall->active) continue;
        if (wall->slot>=10) {
            wall->distance-=w->speed;
            if (wall->distance<1) {
                wall->distance=0;
                if (wall->slot==20 || wall->slot==21) {
                    w->morph_state=wall->slot==20 ? 1:2;
                    w->marker_wait=0;
                } else if (wall->slot==22) w->camera_trigger=1;
                wall->active=0;
            }
        } else pc_move_wall(wall,(int16_t)w->speed);
    }
    while (w->count && !w->walls[w->count-1].active) --w->count;
}
