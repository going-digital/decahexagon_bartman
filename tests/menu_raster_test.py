#!/usr/bin/env python3
"""Compare complete cached menu bitmaps against the previous pixel renderer."""
from pathlib import Path
import subprocess,tempfile
root=Path(__file__).resolve().parents[1]
with tempfile.TemporaryDirectory() as directory:
 p=Path(directory)
 for variant,path in [('before',root/'tests/fixtures/menu_raster_reference.c'),('after',root/'hud.c')]:
  s=path.read_text()
  if variant=='after':s=s[s.index('static const UBYTE front_font'):s.index('#pragma GCC pop_options',s.index('static const UBYTE front_font'))]
  header='''#include <string.h>
#include "menu_font16.h"
#include "credit_qr.h"
#include "arcade.h"
const Arcade *game_arcade(void);
typedef unsigned char UBYTE;typedef unsigned short UWORD;typedef unsigned int ULONG;
static UBYTE *front_planes;
static struct {unsigned record_seconds,record_subsecond_frames;} gamestate;
#define SCREEN_WIDTH 320
#define SCREEN_WIDTH_BYTES 40
unsigned game_selected_profile(void);unsigned game_selection_locked(void);unsigned game_load_failed(void);
unsigned game_front_page(void);unsigned game_front_choice(void);unsigned game_credit_page(void);
void blit_cls(void*p){memset(p,0,8000);}void blit_wait(void){}
'''
  (p/(variant+'.c')).write_text(header+s)
  subprocess.run(['cc','-std=c99','-O2','-I'+str(root),'-Dhud_draw_front='+variant+'_draw','-Dblit_cls='+variant+'_clear','-Dblit_wait='+variant+'_wait','-c',str(p/(variant+'.c')),'-o',str(p/(variant+'.o'))],check=True)
 (p/'test.c').write_text('''#include <assert.h>
#include "arcade.h"
static Arcade arcade;
const Arcade *game_arcade(void){return &arcade;}
#include <string.h>
static unsigned page,choice,credits;
unsigned game_selected_profile(void){return 0;}unsigned game_selection_locked(void){return 0;}unsigned game_load_failed(void){return 0;}
unsigned game_front_page(void){return page;}unsigned game_front_choice(void){return choice;}unsigned game_credit_page(void){return credits;}
void before_draw(void*);void after_draw(void*);
int main(void){unsigned char a[8064],b[8064];
 for(page=1;page<2;page++)for(choice=0;choice<3;choice++)for(credits=0;credits<5;credits++){
 memset(a,0xa5,sizeof(a));memset(b,0xa5,sizeof(b));memset(a+32,0,8000);memset(b+32,0,8000);
 before_draw(a+32);after_draw(b+32);assert(!memcmp(a,b,sizeof(a)));
 }return 0;}
''')
 subprocess.run(['cc','-I'+str(root),str(p/'test.c'),str(p/'before.o'),str(p/'after.o'),'-o',str(p/'test')],check=True)
 subprocess.run([str(p/'test')],check=True)
print('15 Options states: bitmaps identical, including guard bytes')
