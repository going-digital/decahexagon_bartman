#!/usr/bin/env python3
"""Exercise the production ambient update through menu-to-ending zoom changes."""
import subprocess
import tempfile
from pathlib import Path
root=Path(__file__).resolve().parents[1]
source=(root/'game.c').read_text()
start=source.index('static void update_ambient(void)')
end=source.index('\nstatic void project_state',start)
assert 'if(mode!=MODE_ENDING)update_ambient()' not in source
code='''
#include <assert.h>
typedef short WORD;
#define MODE_ENDING 5
#define MUSIC_FIB_STREAM 1
static unsigned mode,zoom_base,cues,pulses;
static struct {unsigned draw_distance_target,draw_distance,pulse;} gamestate;
static unsigned fib_stream_cue(void){++cues;return 1;}
static unsigned patterns_stage(void){return 0;}
static unsigned pc_pulse_tick(unsigned p,int c,unsigned s){(void)p;(void)c;(void)s;++pulses;return 12;}
'''+source[start:end]+'''
int main(void){
 mode=MODE_ENDING;zoom_base=1200;gamestate.draw_distance_target=400;
 gamestate.pulse=12;
 for(unsigned i=0;i<90;++i) update_ambient();
 assert(gamestate.draw_distance==400 && !gamestate.pulse && !cues && !pulses);
 zoom_base=200;gamestate.draw_distance_target=400;
 for(unsigned i=0;i<90;++i) update_ambient();
 assert(gamestate.draw_distance>=393 && gamestate.draw_distance<=400);
 mode=0;update_ambient();assert(cues==1 && pulses==1 && gamestate.pulse==12);
 return 0;
}
'''
with tempfile.TemporaryDirectory() as d:
 p=Path(d);(p/'test.c').write_text(code)
 subprocess.run(['cc','-std=c99','-Wall','-Wextra','-Werror',str(p/'test.c'),'-o',str(p/'test')],check=True)
 subprocess.run([str(p/'test')],check=True)
print('PASS: ending camera eases both directions without gameplay cues; ordinary pulse retained')
