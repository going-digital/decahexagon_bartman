#!/usr/bin/env python3
"""Exercise the actual Arcade menu/death boundary and normal-record isolation."""
from pathlib import Path
import subprocess,tempfile
root=Path(__file__).resolve().parents[1]
s=(root/'game.c').read_text()
def function(name):
    start=s.index(name+'(');start=s.rfind('\n',0,start)+1
    end=s.index('{',start)+1;depth=1
    while depth:
        depth+=(s[end]=='{')-(s[end]=='}');end+=1
    return s[start:end].replace('__attribute__((optimize("Os"))) ','')
code='''
#include <assert.h>
#include <string.h>
#include "arcade.h"
#include "front_menu.h"
#include "pc_menu.h"
#include "pc_time.h"
#include "pc_lifecycle.h"
typedef unsigned char UBYTE;
typedef unsigned short UWORD;
typedef struct {unsigned char text[2],accept_edge,back_edge,fire_edge,held;} InputState;
enum {MODE_ATTRACT,MODE_PLAYING,MODE_DEAD,MODE_GAMEOVER};
static unsigned mode;
static Arcade arcade;
static unsigned arcade_submitted,selected_profile,test_run,save_dirty,new_record;
static PcRecords records;
static PcLifecycle lifecycle;
static struct FrontMenu front;
static struct {UWORD time_seconds,time_subsecond_frames,record_seconds,record_subsecond_frames;} gamestate;
static void sfx_emit(unsigned n){(void)n;}
static void set_mode(unsigned n){mode=n;}
'''
code+='\n'.join(function(n) for n in ['game_menu_locks','game_selection_locked','load_record','record_time','update_arcade_menu'])
code+='''
int main(void){
    InputState in={0};front.page=FRONT_OPTIONS;
    records.best[0]=100;records.completed[1]=1;PcRecords before=records;
    in.fire_edge=1;assert(update_arcade_menu(&in));assert(arcade.enabled);
    assert(!game_menu_locks());
    for(selected_profile=0;selected_profile<6;++selected_profile)assert(!game_selection_locked());
    selected_profile=0;mode=MODE_PLAYING;gamestate.time_seconds=180;record_time();
    assert(!memcmp(&records,&before,sizeof records) && !save_dirty);
    mode=MODE_DEAD;in=(InputState){0};lifecycle.elapsed=1234;lifecycle.extent=200;
    assert(!update_arcade_menu(&in));
    lifecycle.extent=320;in.fire_edge=1;assert(update_arcade_menu(&in));
    assert(mode==MODE_ATTRACT && front.page==FRONT_NAME && arcade.pending);
    assert(arcade.scores[0][0].ticks==1234);
    in=(InputState){.text={'A','B'}};assert(update_arcade_menu(&in));
    in=(InputState){.text={' ',0},.fire_edge=1};assert(update_arcade_menu(&in));
    assert(arcade.pending); /* Space must type, not accept. */
    in=(InputState){.text={'C',0},.accept_edge=1};assert(update_arcade_menu(&in));
    assert(!arcade.pending && front.page==FRONT_LEVELS);
    assert(!strcmp(arcade.scores[0][0].name,"AB C"));
    assert(gamestate.record_seconds==20 && gamestate.record_subsecond_frames==34);
    mode=MODE_GAMEOVER;assert(!update_arcade_menu(&in)); /* never insert same run twice */
    mode=MODE_DEAD;arcade_submitted=0;test_run=1;assert(!update_arcade_menu(&in));
    test_run=0;mode=MODE_ATTRACT;front.page=FRONT_OPTIONS;
    in=(InputState){.fire_edge=1};assert(update_arcade_menu(&in));
    assert(!arcade.enabled && game_menu_locks()==((1<<3)|(1<<5)));
    assert(!memcmp(&records,&before,sizeof records));
    assert(gamestate.record_seconds==1 && gamestate.record_subsecond_frames==40);
    assert(update_arcade_menu(&in));assert(arcade.scores[0][0].ticks==1234); /* toggle preserves table */
    return 0;
}
'''
with tempfile.TemporaryDirectory() as d:
    p=Path(d);(p/'test.c').write_text(code)
    subprocess.run(['cc','-std=c99','-Wall','-Wextra','-Werror','-I'+str(root),str(p/'test.c'),str(root/'pc_lifecycle.c'),str(root/'pc_menu.c'),'-o',str(p/'test')],check=True)
    subprocess.run([str(p/'test')],check=True)
print('Arcade flow: toggle, six unlocks, isolated normal records, qualifying death, name input, retry exclusion and return pass')
