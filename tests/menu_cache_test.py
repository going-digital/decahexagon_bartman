#!/usr/bin/env python3
from pathlib import Path
import subprocess,tempfile
root=Path(__file__).resolve().parents[1];s=(root/'hud.c').read_text()
a=s.index('void *hud_front_bitmap(');b=s.index('\nUBYTE hud_front_cached',a)
code='''#include <assert.h>
#include "arcade.h"
static Arcade arcade;
const Arcade *game_arcade(void){return &arcade;}
static unsigned short front_versions[3];
typedef unsigned short UWORD;
#include <string.h>
typedef unsigned char UBYTE;typedef unsigned long ULONG;
#define BITPLANE_SIZE 8000
static UBYTE storage[24000],*front_planes=storage;
static struct {unsigned record_seconds,record_subsecond_frames;} gamestate;
static ULONG front_scores[3];
static int front_positions[3];
static unsigned front_strip_page;
void menu_strip_init_for(UBYTE*p,unsigned n){(void)p;(void)n;}
unsigned game_menu_locks(void){return 0;}
unsigned game_selected_profile(void){return 0;}
unsigned game_selection_locked(void){return 0;}
unsigned game_load_failed(void){return 0;}
int game_front_slide(void){return 0;}
void menu_strip_window(const UBYTE*s,UBYTE*d,unsigned p){(void)s;(void)d;(void)p;}
static ULONG front_keys[3]={~0u,~0u,~0u};
static unsigned page=1,choice,credits,visible=1,draws;
unsigned game_front_page(void){return page;}
unsigned game_front_choice(void){return choice;}
unsigned game_credit_page(void){return credits;}
unsigned game_front_visible(void){return visible;}
static unsigned copies;
void blit_copy_plane(const void*s,void*d){++copies;memcpy(d,s,BITPLANE_SIZE);}
void blit_wait(void){}
void hud_draw_front(void*p){++draws;((UBYTE*)p)[0]=choice+1;}
'''+s[a:b]+'''
int main(void){
 for(unsigned i=0;i<300;++i)assert(hud_front_bitmap(i%3)==storage+(i%3)*8000);
 assert(draws==1 && copies==2);
 credits=1;choice=1;hud_front_bitmap(0);assert(draws==2 && storage[0]==2);
 assert(storage[8000]==1 && storage[16000]==1);
 hud_front_bitmap(0);assert(draws==2);
 hud_front_bitmap(1);hud_front_bitmap(2);assert(draws==2 && copies==4);
 visible=0;assert(!hud_front_bitmap(0));assert(draws==2 && copies==4);
 visible=1;hud_front_bitmap(0);assert(draws==2 && copies==4);
 page=2;credits=4;hud_front_bitmap(0);assert(draws==3);
 page=0;hud_front_bitmap(0);hud_front_bitmap(1);hud_front_bitmap(2);assert(draws==6);
 choice=2;hud_front_bitmap(0);hud_front_bitmap(1);hud_front_bitmap(2);assert(draws==6);
 page=3;hud_front_bitmap(0);assert(draws==7);
 gamestate.record_seconds=61;hud_front_bitmap(0);assert(draws==8);
 hud_front_bitmap(1);assert(draws==8);
 gamestate.record_subsecond_frames=30;hud_front_bitmap(1);assert(draws==9);
 front_planes=0;assert(!hud_front_bitmap(1));assert(draws==9);
 return 0;}
'''
with tempfile.TemporaryDirectory() as d:
 p=Path(d);(p/'test.c').write_text(code)
 subprocess.run(['cc','-std=c99','-Wall','-Wextra','-Werror','-I'+str(root),str(p/'test.c'),'-o',str(p/'test')],check=True)
 subprocess.run([str(p/'test')],check=True)
print('Menu cache: unchanged frames do not redraw; slot ownership, invalidation and fallback pass')
