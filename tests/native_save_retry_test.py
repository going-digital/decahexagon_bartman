#!/usr/bin/env python3
"""Test native save retry orchestration, using real disk transactions with mocked hardware/sector I/O."""
from pathlib import Path
import re
import subprocess
import tempfile
root = Path(__file__).resolve().parents[1]
source = (root/'trackloader/native_save.c').read_text()
source = '\n'.join(line for line in source.splitlines() if not line.startswith('#include'))
source, count = re.subn(r'__asm volatile\(.*?\);', '(void)sr;', source, flags=re.S)
assert count == 4
source = source.replace('unsigned short sr;', 'unsigned short sr=0;')
prelude = r'''
#include <assert.h>
#include <stdint.h>
#include <string.h>
#include "trackloader/save_disk.h"
#include "include/hardware/intbits.h"
typedef uintptr_t ULONG;
typedef unsigned short UWORD;
enum {MODE_PLAYING,MODE_ATTRACT,MODE_GAMEOVER};
typedef struct {int (*disk_transfer)(unsigned,unsigned,void*);void *save_scratch;
 const unsigned char *save_identity,*save_anchors;} TrackGameBoot;
static struct {UWORD dmaconr,intenar,intena,dmacon,copjmp1,intreq;ULONG cop1lc;} hw,*custom=&hw;
static unsigned char copper_dispatch[4];
static unsigned mode=MODE_GAMEOVER,dirty=1,snapshots,commits,acks,error,begins;
static unsigned protected_disk=1,fail_readback,writes;
static unsigned char slots[2][512],identity_bytes[16];
static TrackSave current,submitted;
static int game_mode(void){return mode;}
static int game_save_dirty(void){return dirty;}
static int game_save_snapshot(TrackSave *s){snapshots++;if(!dirty)return 0;*s=current;return 1;}
static int game_save_committed(const TrackSave *s){acks++;assert(s->generation==current.generation);
 dirty=s->records.best[0]!=current.records.best[0];current.generation++;return 1;}
static void hud_save_failed(unsigned value){error=value;}
static void blit_wait(void){}
static void WaitDisplayList(void){}
static void fib_stream_stop(void){}
static void sfx_shutdown(void){}
static void sfx_init(void){}
static unsigned busy;
static void hud_loading_begin(void){assert(!busy);busy=1;}
static void hud_loading_end(void){assert(busy);busy=0;}
static void *hud_loading_copper(void *p,unsigned saving){assert(saving);return p;}
static int io(unsigned op,unsigned sector,void *buffer){
 if(op==2){begins++;commits++;return 1;}
 if(sector==1738 || sector==1749){
  unsigned index=sector==1749;
  if(op==0){memcpy(buffer,slots[index],512);if(fail_readback==2){fail_readback=0;return 0;}return 1;}
  assert(op==1);assert(track_save_decode(buffer,identity_bytes,&submitted));
  if(protected_disk)return 0;
  memcpy(slots[index],buffer,512);writes++;
  if(fail_readback==1)fail_readback=2;
  return 1;
 }
 assert(op==0 && sector==0);memset(buffer,0,512);return 1;
}

'''
checks = r'''
int main(void){
 TrackSaveDiskScratch workspace;unsigned char id[16]={0},anchors[1548]={0};
 TrackGameBoot boot={io,&workspace,id,anchors};native_save_init(&boot);
 current.generation=1;current.records.best[0]=359;
 assert(native_save_tick(0)==1 && error && dirty && commits==1 && !acks && snapshots==1 && !writes);
 assert(!native_save_tick(0) && commits==1);
 mode=MODE_PLAYING;assert(!native_save_tick(0) && !error);
 current.records.best[0]=500;mode=MODE_GAMEOVER;
 assert(native_save_tick(0)==1 && commits==2 && !acks && snapshots==1 && !writes);
 assert(submitted.records.best[0]==359 && submitted.generation==1);
 /* Reinsert writable media, then lose the first write's readback. */
 mode=MODE_PLAYING;native_save_tick(0);mode=MODE_GAMEOVER;protected_disk=0;fail_readback=1;
 assert(native_save_tick(0)==1 && !acks && dirty && error && writes==1);
 TrackSave decoded;
 assert(track_save_decode(slots[0],id,&decoded) && decoded.generation==1 && decoded.records.best[0]==359);
 mode=MODE_PLAYING;native_save_tick(0);mode=MODE_GAMEOVER;
 assert(native_save_tick(0)==1 && acks==1 && dirty && !error && writes==1);
 /* Already-written snapshot acknowledged without rewriting; newer RAM follows. */
 assert(native_save_tick(0)==1 && acks==2 && !dirty && snapshots==2 && writes==2);
 assert(track_save_decode(slots[1],id,&decoded) && decoded.generation==2 && decoded.records.best[0]==500);
 assert(track_save_select(slots[0],slots[1],id,&decoded)==1 && decoded.records.best[0]==500);
 assert(!native_save_tick(0) && commits==5 && begins==5);
 return 0;
}
'''
with tempfile.TemporaryDirectory() as tmp:
    p=Path(tmp);(p/'test.c').write_text(prelude+source+checks)
    subprocess.run(['cc','-std=c11','-Wall','-Wextra','-Werror','-I'+str(root),str(p/'test.c'),str(root/'trackloader/save.c'),str(root/'trackloader/save_disk.c'),'-o',str(p/'test')],check=True)
    subprocess.run([str(p/'test')],check=True)
print('Native retry with real codec/transactions: failure retains pending snapshot; retry acknowledgement preserves newer progress; next generation commits')
