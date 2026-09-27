#!/usr/bin/env python3
"""Compare the removed projection/collision sequence using production functions."""
from pathlib import Path
import subprocess, json
root=Path(__file__).resolve().parents[2]
out=root/'scratchpad/performance_projection';out.mkdir(exist_ok=True)
source=(root/'game.c').read_text()
start=source.index('static void project_state(void)');end=source.index('\n}',start)+2
projection=source[start:end]
code=r'''
#include "pc_morph.h"
#include <stdio.h>
#include <string.h>
static PcMorph morph;
static PcPlayer player;
static struct {uint16_t num_sides,segment_angle,player_angle;} gamestate;
'''+projection+r'''
int main(void) {
 unsigned cases=0,mismatches=0,blocked=0,hits=0;
 const int distances[]={0,100,122,123,144,145,150,151};
 const int widths[]={199,200,400};
 const int speeds[]={22,35,40};
 for(unsigned sides=3;sides<=6;++sides)
 for(unsigned phase=0;phase<=11;++phase)
 for(unsigned growing=0;growing<2;++growing)
 for(unsigned angle=0;angle<360;++angle)
 for(unsigned d=0;d<8;++d)
 for(unsigned w=0;w<3;++w)
 for(unsigned speed=0;speed<3;++speed) {
  morph=(PcMorph){sides,phase,growing};
  PcPlayer initial={(int16_t)angle,(int16_t)((angle+37)%360),0,0};
  PcWall walls[2]={{distances[d],widths[w],angle/(360/sides),1},
                    {distances[d],widths[w],((angle+37)%360)/(360/sides),1}};
  player=initial;memset(&gamestate,0xa5,sizeof(gamestate));
  project_state();
  pc_collide(&player,walls,2,sides,speeds[speed]);
  project_state();
  PcPlayer expected=player;
  uint16_t ns=gamestate.num_sides,sa=gamestate.segment_angle,pa=gamestate.player_angle;
  player=initial;memset(&gamestate,0x5a,sizeof(gamestate));
  pc_collide(&player,walls,2,sides,speeds[speed]);
  project_state();
  ++cases;blocked+=player.blocked!=0;hits+=player.hit!=0;
  if(player.angle!=expected.angle || player.previous_angle!=expected.previous_angle ||
     player.hit!=expected.hit || player.blocked!=expected.blocked ||
     gamestate.num_sides!=ns || gamestate.segment_angle!=sa || gamestate.player_angle!=pa)
   ++mismatches;
 }
 printf("{\"cases\":%u,\"mismatches\":%u,\"blocked_cases\":%u,\"hit_cases\":%u}\n",cases,mismatches,blocked,hits);
 return mismatches!=0;
}
'''
(out/'equivalence.c').write_text(code)
subprocess.run(['cc','-O2','-Wall','-Wextra','-Werror','-I'+str(root),str(out/'equivalence.c'),str(root/'pc_core.c'),str(root/'pc_morph.c'),'-o',str(out/'equivalence')],check=True)
r=json.loads(subprocess.check_output([str(out/'equivalence')],text=True))
r['scope']='Host comparison of production project_state and pc_collide: before vs after the removed call; all projected fields and player state, not a whole-game trace. Sides3..6, phases0..11, both morph directions, all360 player angles, 8 distance boundaries, 3 widths, 3 speeds; two active walls exercise rollback and hit cases.'
(root/'docs/PROJECTION_EQUIVALENCE.json').write_text(json.dumps(r,indent=2)+'\n');print(json.dumps(r,indent=2))
