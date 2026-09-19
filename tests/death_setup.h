#include "../pc_death.h"
static void death_setup(PcLifecycle *life_out,PcWorld *w_out,PcMorph *m_out,int initial,int shape) {
 const int walls[6][4]={{0,1000,200,1},{4,0,77,1},{20,0,0,1},{21,-5,0,1},{22,500,0,0},{3,2200,333,0}};
  PcLifecycle *life=life_out;PcMorph *m=m_out;PcWorld *w=w_out;
  *life=(PcLifecycle){123,40,initial?59:1};pc_morph_reset(m);pc_world_reset(w);
  m->sides=shape==2 || shape==5 ? 4:shape==3 ? 3:6;
  w->morph_state=shape==1 || shape==4 ? 1:shape==2 ? 2:shape==5 ? 3:0;
  if(shape==4)m->phase=5;
  if(shape==5){m->phase=7;m->growing=1;}
  w->marker_wait=1;w->count=6;
  for(int i=0;i<6;i++){w->walls[i].slot=walls[i][0];w->walls[i].distance=walls[i][1];w->walls[i].width=walls[i][2];w->walls[i].active=walls[i][3];}
  if(shape&1){w->walls[1].active=0;w->walls[5].active=1;}
}
static uint32_t death_hash(const PcWorld *w) {
 uint32_t hash=2166136261u;
 for(int i=0;i<6;i++){const PcWall *wall=&w->walls[i];hash=(hash^wall->slot)*16777619u;hash=(hash^(uint32_t)wall->distance)*16777619u;hash=(hash^(uint32_t)wall->width)*16777619u;hash=(hash^wall->active)*16777619u;}
 return hash;
}
