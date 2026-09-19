#include <stdio.h>
#include "death_setup.h"
int main(void) {
 static PcWorld w;
 for(int initial=0;initial<2;initial++)for(int profile=0;profile<12;profile++)for(int shape=0;shape<6;shape++) {
  PcLifecycle life;PcMorph m;death_setup(&life,&w,&m,initial,shape);
  if(profile>=6)life.elapsed=profile==8 || profile==9 ? 20000:14000;
  for(int tick=1;tick<=120;tick++) {
   pc_death_tick(&life,&w,&m,profile<6 ? profile%3:profile<8 ? 1:2,profile<6 ? profile%3:profile<10 ? 0:1);
   uint32_t hash=death_hash(&w);
   printf("%d %d %d %d %u %u %u %u %u %u %u %u %u\n",initial,profile,shape,tick,life.death,life.extent,m.sides,w.morph_state,pc_morph_arc(&m),w.marker_wait,hash,life.elapsed,w.count);
  }
 }
 return 0;
}
