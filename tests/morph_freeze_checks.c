#include "../pc_morph.h"
static const struct {
 uint16_t marker_slot,sides_seed,tick,state,sides,freeze;
 uint32_t hash;
 uint16_t count;
} cases[]={
#include "morph_freeze_cases.inc"
};
static uint32_t morph_freeze_hash(const PcWorld *w) {
 uint32_t hash=2166136261u;
 for(int i=0;i<2;i++){const PcWall *wall=&w->walls[i];
  hash=(hash^(uint32_t)wall->slot)*16777619u;hash=(hash^(uint32_t)wall->distance)*16777619u;
  hash=(hash^(uint32_t)wall->width)*16777619u;hash=(hash^wall->active)*16777619u;}
 return hash;
}
/* Verifies pc_world_move stalls (this+0x54c0 in the reference) exactly while
 * pc_morph_tick is refreshing the freeze for morph_state 1/2/3, including the
 * "already six sides" grow no-op, and resumes once it decays to zero. See
 * tools/probe_pc_morph_freeze.c for how these native traces were captured. */
unsigned pc_morph_freeze_checks(void) {
 static PcWorld w;static PcMorph m;
 for(unsigned i=0;i<sizeof(cases)/sizeof(cases[0]);++i) {
  if(cases[i].tick==1) {
   pc_world_reset(&w);pc_morph_reset(&m);m.sides=(uint8_t)cases[i].sides_seed;
   w.count=2;
   w.walls[0]=(PcWall){2000,50,0,1};
   w.walls[1]=(PcWall){22,0,(uint8_t)cases[i].marker_slot,1};
  }
  pc_morph_tick(&m,&w);
  pc_world_move(&w);
  if(w.morph_state!=cases[i].state || m.sides!=cases[i].sides || w.freeze!=cases[i].freeze ||
     morph_freeze_hash(&w)!=cases[i].hash || w.count!=cases[i].count) return 800+i;
 }
 /* A retry/new world must not inherit a partially elapsed freeze. */
 w.freeze=19;pc_world_reset(&w);pc_morph_reset(&m);
 pc_world_add(&w,0,2000,50);pc_world_move(&w);
 if(w.freeze || w.walls[0].distance!=1978)return 1100;
 /* Death-only regrowth must not restart an existing freeze countdown. */
 m.sides=4;w.morph_state=4;w.freeze=3;
 for(unsigned tick=0;tick<15;++tick) {
  pc_morph_tick(&m,&w);
  unsigned expected=tick<3?2-tick:0;
  if(w.freeze!=expected)return 1101+tick;
 }
 if(m.sides!=5 || w.morph_state)return 1120;
 return 0;
}
