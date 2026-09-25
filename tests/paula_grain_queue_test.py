#!/usr/bin/env python3
"""Exercise the production Paula queue/envelope ISR against fake registers."""
import subprocess,tempfile
from pathlib import Path
root=Path(__file__).resolve().parents[1]
s=(root/'tests/fib_stream.c').read_text()
a=s.index('static void queue_grain(');b=s.index('static void audio_irq(',a)
code='''
#include <assert.h>
#include <stdint.h>
#define FIB_BUFFERS 4
#define INTF_AUD3 1024
#define UWORD unsigned
#define ULONG uintptr_t
static unsigned char storage[2048],*buffers=storage;
static unsigned playing,pending,next_queue,first_irq,grain_step,pending_step,blocks,underruns;
static unsigned state[4],positions[4];
static struct {struct {uintptr_t ac_ptr;unsigned ac_len,ac_vol;} aud[4];unsigned intreq;} hw,*custom=&hw;
'''+s[a:b]+'''
int main(void){
 first_irq=1;next_queue=1;state[0]=2;
 for(unsigned i=1;i<4;i++){state[i]=1;positions[i]=i*256;}
 queue_grain(0,0);
 for(unsigned tick=0;tick<64;tick++){
   grain_irq();
   unsigned slot=tick/16,step=tick%16;
   assert(playing==slot && grain_step==step);
   assert(hw.aud[0].ac_vol+hw.aud[3].ac_vol==64);
   assert(hw.aud[3].ac_vol==(slot?step*4:0));
   assert(hw.aud[0].ac_len==8 && hw.aud[3].ac_len==8);
   assert(hw.aud[3].ac_ptr-hw.aud[0].ac_ptr==256);
   assert(hw.aud[0].ac_ptr>=(uintptr_t)storage && hw.aud[3].ac_ptr<(uintptr_t)(storage+2048));
   if(slot)assert(state[slot-1]==0);
 }
 assert(underruns==1); /* no producer: repeat immutable last hop */
 grain_irq();assert(playing==3 && state[3]==2 && grain_step==0);
 state[0]=1;positions[0]=1024;
 for(unsigned i=0;i<16;i++)grain_irq();
 assert(playing==0 && state[3]==0 && state[0]==2);
 return 0;
}
'''
with tempfile.TemporaryDirectory() as d:
 p=Path(d);(p/'test.c').write_text(code)
 subprocess.run(['cc','-std=c99','-Wall','-Wextra','-Werror',str(p/'test.c'),'-o',str(p/'test')],check=True)
 subprocess.run([str(p/'test')],check=True)
print('PASS: hardware envelope, next-reload pointers, ownership, underrun and recovery')
