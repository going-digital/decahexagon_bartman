#include "../pc_lifecycle.h"
static const struct { uint16_t retry,tick,death,extent,elapsed; } cases[]={
#include "lifecycle_cases.inc"
};
unsigned pc_lifecycle_checks(void) {
    unsigned index=0;
    for (unsigned retry=0;retry<3;++retry) {
        PcLifecycle p={123,40,1};PcPlayer player={137,137,1,1};
        for (unsigned tick=1;tick<=85;++tick) {
            if ((retry==1 || (retry==2 && tick==72)) && pc_lifecycle_can_start(&p)) {
                pc_lifecycle_start(&p,&player);
                if (player.hit || player.blocked || player.angle!=137 || player.previous_angle!=137)
                    return 800;
            }
            pc_lifecycle_tick(&p);
            if (index<sizeof(cases)/sizeof(cases[0]) && cases[index].retry==retry && cases[index].tick==tick) {
                if (p.death!=cases[index].death || p.extent!=cases[index].extent || p.elapsed!=cases[index].elapsed)
                    return 801+index;
                ++index;
            }
        }
    }
    return index==sizeof(cases)/sizeof(cases[0]) ? 0:830;
}
