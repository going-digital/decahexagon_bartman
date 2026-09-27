/* Differential oracle: original collision algorithm before reciprocal optimisation. */
#include "../pc_core.h"
void reference_collide(PcPlayer *player, const PcWall *walls, uint16_t count,
                uint8_t sides, int16_t speed) {
    int16_t arc = 360 / sides;
    int16_t slot = player->angle / arc;
    player->blocked = 0;
    for (uint16_t i = 0; i < count; ++i) {
        const PcWall *w = &walls[i];
        if (!w->active || w->slot != slot || w->distance >= 151) continue;
        if (w->distance >= 145 - speed) player->hit = 1;
        else if (w->width > 199) {
            player->angle = player->previous_angle;
            slot = player->angle / arc;
            player->blocked = 1;
        }
    }
    player->previous_angle = player->angle;
}

#include "../pc_core.h"
#include <assert.h>
#include <stdio.h>
void reference_collide(PcPlayer*,const PcWall*,uint16_t,uint8_t,int16_t);
int main(void){
 unsigned cases=0;
 for(unsigned sides=3;sides<=6;++sides)
 for(unsigned a=0;a<65536;++a)
 for(unsigned mode=0;mode<4;++mode) {
  PcPlayer before={(int16_t)a,(int16_t)(a+37),0,0},after=before;
  PcWall walls[6];
  for(unsigned i=0;i<6;++i)walls[i]=(PcWall){mode==0?151:mode==1?150:100,mode==3?199:400,i,1};
  reference_collide(&before,walls,6,sides,35);pc_collide(&after,walls,6,sides,35);
  assert(before.angle==after.angle && before.previous_angle==after.previous_angle && before.hit==after.hit && before.blocked==after.blocked);++cases;
 }
 printf("%u collision cases match\n",cases);
}
