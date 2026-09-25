#!/usr/bin/env python3
"""Exercise actual stream dispatch: bonus survives play, stops on death/retry."""
import subprocess,tempfile
from pathlib import Path
root=Path(__file__).resolve().parents[1]
s=(root/'tests/fib_stream.c').read_text()
f=s[s.index('void fib_stream_tick('):s.index('#if AUDIO_DIAGNOSTICS')]
code='''
#include <assert.h>
#include "pcm_lifecycle.h"
static unsigned stretch_mode,stretch_gameplay,reverse_mode,running,stops,starts;
static PcmLifecycle lifecycle;
static struct {struct {unsigned ac_vol;} aud[1];} hw,*custom=&hw;
static void fib_stream_stop(void){++stops;stretch_mode=stretch_gameplay=reverse_mode=running=0;}
static void fib_stream_start(void){++starts;running=1;}
'''+f+'''
int main(void){
 reverse_mode=running=1;
 fib_stream_tick(0,0);assert(running && reverse_mode);
 fib_stream_tick(0,1);assert(!running && !reverse_mode);stops=0;

 stretch_mode=stretch_gameplay=running=1;
 for(unsigned i=0;i<200;++i)fib_stream_tick(1,0);
 assert(!stops && !starts && running && stretch_mode);
 fib_stream_tick(0,0);assert(stops==1 && !running);
 fib_stream_tick(1,0);assert(starts==1 && running && !stretch_mode);
 fib_stream_tick(0,1);assert(!running);
 stretch_mode=running=1;stretch_gameplay=0;
 fib_stream_tick(0,0);assert(running);
 fib_stream_tick(0,1);assert(!running);
 return 0;
}
'''
with tempfile.TemporaryDirectory() as d:
 p=Path(d);(p/'test.c').write_text(code)
 subprocess.run(['cc','-std=c99','-Wall','-Wextra','-Werror','-I'+str(root),str(p/'test.c'),str(root/'pcm_lifecycle.c'),'-o',str(p/'test')],check=True)
 subprocess.run([str(p/'test')],check=True)
print('PASS: bonus playback sustained; death/menu stop; retry restores ordinary music')
