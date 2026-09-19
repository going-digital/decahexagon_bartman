#include <stdio.h>
#include "../pc_lifecycle.h"
int main(void) {
 for(int entry=0;entry<2;entry++)for(int profile=0;profile<6;profile++)for(int angle=0;angle<360;angle+=59) {
  PcLifecycle p;PcPlayer player={angle,angle,1,1};
  pc_lifecycle_init(&p);p.death=80;p.elapsed=1234;pc_lifecycle_start(&p,&player);
  printf("S %d %d %d %u %d %d %u %u\n",entry,profile,angle,p.death,player.previous_angle,player.angle,p.elapsed,p.extent);
  pc_lifecycle_tick(&p);
  printf("F %d %d %d %u %d %u %u\n",entry,profile,angle,p.death,player.angle,p.elapsed,p.extent);
 }
 for(int profile=0;profile<6;profile++)for(int retry=0;retry<3;retry++) {
  PcLifecycle p={123,40,1};PcPlayer player={137,137,1,1};
  for(int tick=1;tick<=85;tick++) {
   if ((retry==1 || (retry==2 && tick==72)) && pc_lifecycle_can_start(&p)) {
    pc_lifecycle_start(&p,&player);
   }
   pc_lifecycle_tick(&p);
   printf("D %d %d %d %u %u %u %d\n",profile,retry,tick,p.death,p.extent,p.elapsed,player.angle);
  }
 }
 return 0;
}
