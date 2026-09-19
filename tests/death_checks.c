#include "death_setup.h"
static const struct {
 uint16_t initial,profile,shape,tick,death,extent,sides,state,arc,wait;
 uint32_t hash,elapsed;
 uint16_t count;
} cases[]={
#include "death_cases.inc"
};
unsigned pc_death_checks(void) {
 static PcWorld w;
 for(unsigned i=0;i<sizeof(cases)/sizeof(cases[0]);++i) {
  PcLifecycle life;PcMorph m;death_setup(&life,&w,&m,cases[i].initial,cases[i].shape);
  if(cases[i].profile>=6)life.elapsed=cases[i].profile==8 || cases[i].profile==9 ? 20000:14000;
  for(unsigned t=0;t<cases[i].tick;++t)pc_death_tick(&life,&w,&m,cases[i].profile<6 ? cases[i].profile%3:cases[i].profile<8 ? 1:2,
      cases[i].profile<6 ? cases[i].profile%3:cases[i].profile<10 ? 0:1);
  if(life.death!=cases[i].death || life.extent!=cases[i].extent || m.sides!=cases[i].sides ||
     w.morph_state!=cases[i].state || pc_morph_arc(&m)!=cases[i].arc || w.marker_wait!=cases[i].wait ||
     death_hash(&w)!=cases[i].hash || life.elapsed!=cases[i].elapsed || w.count!=cases[i].count) return 900+i;
 }
 return 0;
}
