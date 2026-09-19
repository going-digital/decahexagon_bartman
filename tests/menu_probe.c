#include <stdio.h>
#include "../pc_menu.h"
int main(void) {
 for(int wedge=0;wedge<6;wedge++)for(int tape=0;tape<4;tape++) {
  PcMenu m;pc_menu_reset(&m,wedge?6-wedge:0);
  for(int tick=0;tick<80;tick++) {
   int held=tape==0?1:tape==1?2:tape==2?3:(tick%23<7?1:tick%23<13?2:0);
   pc_menu_tick(&m,held);
   printf("M %d %d %d %d %d %d %d\n",wedge,tape,tick,m.wedge,m.angle,m.motion,m.cooldown);
  }
 }
 for(int mask=0;mask<8;mask++)for(int wedge=0;wedge<6;wedge++) {
  PcRecords r={0};for(int i=0;i<3;i++)r.completed[i]=(mask>>i)&1;
  int p=wedge?6-wedge:0,ok=pc_profile_unlocked(&r,p);
  printf("U %d %d %d %d %d\n",mask,wedge,ok?p%3:-2,ok?p/3:0,!ok);
 }
 for(int p=0;p<6;p++) {
  PcRecords r={0};pc_record_tick(&r,p,5000+p);
  printf("R %d",p);for(int i=0;i<6;i++)printf(" %u",r.best[i]);
  for(int i=0;i<3;i++)printf(" %u %u",r.completed[i],r.completed[i+3]);
  puts("");
 }
 for(int p=0;p<6;p++)for(int before=3599;before<=3601;before++) {
  PcRecords r={0};pc_record_tick(&r,p,before+1);
  printf("T %d %d %u %u\n",p,before,r.best[p],r.completed[p]);
 }
 return 0;
}
