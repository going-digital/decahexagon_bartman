#!/usr/bin/env python3
"""Exercise the actual level-menu formatter, including empty and capped scores."""
from pathlib import Path
import subprocess,tempfile
root=Path(__file__).resolve().parents[1]
s=(root/'hud.c').read_text();s=s[s.index('void hud_draw_front('):s.index('#pragma GCC pop_options',s.index('static const UBYTE front_font'))]
code=r'''#include <assert.h>
#include "arcade.h"
static Arcade arcade;
const Arcade *game_arcade(void){return &arcade;}
#include "credit_qr.h"
#include <string.h>
typedef unsigned char UBYTE;typedef unsigned int ULONG;
static UBYTE *front_planes;
static struct {unsigned record_seconds,record_subsecond_frames;} gamestate;
static unsigned profile,normal,hyper,locked,locked_name;static char difficulty[64],score[64];
unsigned game_front_page(void){return 3;}unsigned game_front_choice(void){return 0;}
unsigned game_credit_page(void){return 0;}unsigned game_selected_profile(void){return profile;}
unsigned game_load_failed(void){return 0;}unsigned game_selection_locked(void){return locked;}
void front_arcade_scores(UBYTE*p){(void)p;}
void blit_cls(void*p){(void)p;}void blit_wait(void){}
void front_text(UBYTE*p,const char*s,unsigned y,unsigned scale){
 (void)p;(void)y;(void)scale;
 if(!strcmp(s,"LOCKED"))++locked_name;
 if(!strcmp(s,"NORMAL"))++normal;if(!strcmp(s,"HYPER"))++hyper;
 if(!strncmp(s,"DIFFICULTY:",11))strcpy(difficulty,s);
 if(!strncmp(s,"BEST SCORE:",11))strcpy(score,s);
}
'''+s+r'''
int main(void){
 const char *expected[]={"HARD","HARDER","HARDEST","HARDESTEST","HARDESTESTEST","HARDESTESTESTEST"};
 for(profile=0;profile<6;++profile){
  normal=hyper=0;hud_draw_front(0);
  assert(!normal && hyper==(profile>=3));
  assert(!strcmp(difficulty+12,expected[profile]));assert(!strcmp(score,"BEST SCORE: 000.00"));
 }
 profile=0;gamestate.record_seconds=61;gamestate.record_subsecond_frames=30;
 hud_draw_front(0);assert(!strcmp(score,"BEST SCORE: 061.50"));
 gamestate.record_seconds=1000;gamestate.record_subsecond_frames=59;
 hud_draw_front(0);assert(!strcmp(score,"BEST SCORE: 999.98"));
 locked=1;profile=4;normal=hyper=locked_name=0;difficulty[0]=score[0]=0;
 hud_draw_front(0);assert(locked_name==1 && !normal && !hyper && !difficulty[0] && !score[0]);
 return 0;
}
'''
with tempfile.TemporaryDirectory() as d:
 p=Path(d);(p/'test.c').write_text(code)
 subprocess.run(['cc','-std=c99','-O2','-I'+str(root),str(p/'test.c'),'-o',str(p/'test')],check=True)
 subprocess.run([str(p/'test')],check=True)
print('Six difficulty labels, tier heading and best-score formatting pass')
