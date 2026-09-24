#!/usr/bin/env python3
"""Exercise the real run-start boundary with a failing/blocking platform adapter."""
import subprocess
import tempfile
from pathlib import Path
root = Path(__file__).resolve().parents[1]
source = (root / 'game.c').read_text()
def function(name):
    start = source.index(name + '(')
    start = source.rfind('\n', 0, start) + 1
    end = source.index('{', start) + 1
    depth = 1
    while depth:
        depth += (source[end] == '{') - (source[end] == '}')
        end += 1
    return source[start:end]
code = r'''
#include <assert.h>
typedef unsigned char UBYTE;
typedef struct { UBYTE held,fire,fire_edge,back_edge,cheat_held; } InputState;
typedef int (*GameRunPreparer)(UBYTE);
static GameRunPreparer run_preparer;
static UBYTE load_barrier,load_retry_wait,load_failed,selected_profile;
static unsigned starts,resets,updates,requests,allow,mode;
static unsigned sound_events,game_palette;
static struct {unsigned best[6],completed[6];} records;
#define MODE_PLAYING 1
static unsigned pc_sfx_begin(unsigned*a,unsigned b,unsigned c,unsigned d) {
    (void)a;(void)b;(void)c;assert(d==selected_profile);starts++;return 0;
}
static void sfx_emit(unsigned n){(void)n;}
static void reset_run(void){resets++;}
static void set_mode(unsigned n){mode=n;}
static unsigned patterns_effective_score(unsigned n){return n;}
static void pc_palette_tick(unsigned*p,unsigned n){(void)p;(void)n;}
static void update_playing(const InputState*in){assert(!in->held);updates++;}
static int prepare(UBYTE profile){assert(profile==selected_profile);requests++;return allow;}
'''
code += '\n'.join(function(n) for n in ['game_load_failed', 'game_set_run_preparer', 'game_take_load_barrier', 'start_playing'])
code += r'''
int main(void) {
 InputState input={0};input.held=3;input.fire=1;
 /* Default DOS behaviour still starts immediately without a load barrier. */
 start_playing(&input);assert(starts==1 && resets==1 && updates==1);
 assert(!game_take_load_barrier());
 game_set_run_preparer(prepare);
 /* Rejected preparation cannot reset gameplay, emit begin SFX or start audio. */
 for(selected_profile=0;selected_profile<6;selected_profile++) {
  load_retry_wait=0;allow=0;mode=2;
  unsigned before=requests;
  start_playing(&input);
  assert(requests==before+1 && mode==2 && starts==1 && resets==1 && updates==1);
  assert(game_load_failed());
  assert(game_take_load_barrier() && !game_take_load_barrier());
  start_playing(&input);assert(requests==before+1); /* held retry does not reload */
  load_retry_wait=0;allow=1; /* confirmation was released, then pressed again */
  start_playing(&input);assert(mode==MODE_PLAYING && game_take_load_barrier());
  assert(starts==2 && resets==2 && updates==2);
  assert(!game_load_failed());
  starts=resets=updates=1;
 }
 return 0;
}
'''
with tempfile.TemporaryDirectory() as directory:
    path=Path(directory)
    (path/'test.c').write_text(code)
    for cheat in (0,1):
        subprocess.run(['cc','-std=c99','-Wall','-Wextra','-Werror',f'-DCHEAT_MODE={cheat}',str(path/'test.c'),'-o',str(path/'test')],check=True)
        subprocess.run([str(path/'test')],check=True)
print('Game loading: all profiles, failure preservation, retry latch and start ordering pass')
