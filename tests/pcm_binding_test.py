#!/usr/bin/env python3
"""Exercise the actual player's stopped-state binding and buffer ownership code."""
import subprocess,tempfile
from pathlib import Path
root=Path(__file__).resolve().parents[1];source=(root/'tests/fib_stream.c').read_text()
def function(name):
 start=source.index(name+'(');start=source.rfind('\n',0,start)+1
 brace=source.index('{',start);depth=1;end=brace+1
 while depth:
  depth+=(source[end]=='{')-(source[end]=='}');end+=1
 return source[start:end]
code='''#include <assert.h>
#include <stdint.h>
#include "fib_pcm.h"
#include "pcm_lifecycle.h"
typedef unsigned char UBYTE;
typedef unsigned short UWORD;
typedef uintptr_t ULONG;
static PcmSong loaded_song;
static unsigned loaded_samples,loaded_cue_bytes,active_cue_bytes,loaded_cue_lead;
static const UBYTE *loaded_cues,*active_cues;
static UBYTE *loaded_buffers,*buffers;
static UWORD running,external_bank;
static PcmLifecycle lifecycle;
#define FIB_BUFFERS 4
#define INTF_AUD0 128
#define DMAF_AUD0 1
struct Hardware {unsigned intena,dmacon,intreq;struct {unsigned ac_vol;} aud[4];};
static struct Hardware hw,*custom=&hw;
static unsigned frees,irq_detaches;
#if !PCM_EXTERNAL_ONLY
static void FreeMem(void*p,unsigned n){assert(p && n==2048);frees++;}
#endif
static void paula_irq_set(unsigned n,void*p){assert(n==0 && !p);irq_detaches++;}
'''
code+='\n'.join(function(n) for n in ['fib_stream_bind','fib_stream_set_cue_lead','fib_stream_unbind','fib_stream_stop'])
code+='''
int main(void){
 unsigned char bank[8]={0},cues[3]={1,2,3};
 unsigned short dma[1024];PcmSong s={0};s.seq_count=1;
 s.sequence=s.offsets=s.bank=bank;s.seq_index=1;s.output_left=3;
 assert(!fib_stream_bind(&s,512,cues,3,(unsigned char*)dma+1));
 assert(!fib_stream_bind(&s,512,0,3,(unsigned char*)dma));
 assert(!fib_stream_bind(&s,511,cues,3,(unsigned char*)dma));
 assert(fib_stream_bind(&s,512,cues,3,(unsigned char*)dma));
 assert(loaded_cue_lead==612);
 assert(fib_stream_set_cue_lead(0) && loaded_cue_lead==0);
 assert(external_bank && loaded_cues==cues && loaded_cue_bytes==3);
 assert(!loaded_song.seq_index && !loaded_song.output_left);
 buffers=loaded_buffers;running=1;
 assert(!fib_stream_bind(&s,512,0,0,(unsigned char*)dma));
 assert(!fib_stream_unbind());
 assert(!fib_stream_set_cue_lead(582) && loaded_cue_lead==0);
 fib_stream_stop();assert(!running && !buffers && !frees && irq_detaches==1);
 assert(hw.intena==INTF_AUD0 && hw.dmacon==DMAF_AUD0 && !hw.aud[0].ac_vol);
 assert(fib_stream_bind(&s,512,0,0,(unsigned char*)dma));
 assert(!loaded_cues && !loaded_cue_bytes);
 assert(loaded_cue_lead==612);
 assert(fib_stream_set_cue_lead(582) && loaded_cue_lead==582);
 assert(fib_stream_unbind());assert(!fib_stream_set_cue_lead(0));assert(!external_bank && !loaded_buffers);
#if !PCM_EXTERNAL_ONLY
 buffers=(unsigned char*)dma;running=1;fib_stream_stop();assert(frees==1);
 fib_stream_stop();assert(frees==1);
#endif
 return 0;
}
'''
with tempfile.TemporaryDirectory() as d:
 p=Path(d);(p/'test.c').write_text(code)
 for mode in [0,1]:
  subprocess.run(['cc','-std=c99','-Wall','-Wextra','-Werror',f'-DPCM_EXTERNAL_ONLY={mode}','-I'+str(root),str(p/'test.c'),'-o',str(p/'test')],check=True)
  subprocess.run([str(p/'test')],check=True)
print('PCM binding: stopped-state checks, cue ownership, DMA shutdown and buffer lifetime pass')
