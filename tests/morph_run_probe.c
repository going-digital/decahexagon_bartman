#include "pc_schedule.h"
#include "pc_morph.h"
#include <stdio.h>
#include <stdlib.h>
static unsigned seed;
static unsigned draws;
static uint16_t draw(void*c,uint16_t max){(void)c;uint32_t x=seed+(++draws)*0x9e3779b9u;x^=x>>16;x*=0x7feb352du;x^=x>>15;return x%max;}
int main(int argc,char **argv){if(argc!=2)return 2;seed=(unsigned)atoi(argv[1]);PcWorld w;PcMorph m;PcSchedule s;pc_world_reset(&w);pc_morph_reset(&m);pc_schedule_reset(&s);s.shape_counter=5;
 for(unsigned t=1;t<=3600;++t){pc_morph_tick(&m,&w);pc_world_move(&w);pc_schedule_tick(&s,&w,m.sides,t,draw,0);
 printf("%u %u %u %u %u %u %u",t,(unsigned)s.wave,m.sides,w.freeze,w.morph_state,w.speed,draws);if(s.chosen>=0)printf(" %d",s.chosen);uint32_t hash=2166136261u;unsigned active=0;
 for(unsigned j=0;j<w.count;++j){PcWall *wall=&w.walls[j];if(!wall->active)continue;++active;
 hash=(hash^wall->slot)*16777619u;hash=(hash^(uint32_t)wall->distance)*16777619u;hash=(hash^(uint32_t)wall->width)*16777619u;}
 printf(" | %u %u\n",active,hash);}
}
