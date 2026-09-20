#!/usr/bin/env python3
"""Exercise the actual effects driver with mocked registers and DMA boundaries.
The beam wait is replaced with a hook; this is an ordering/lifecycle test, not
an emulation of Paula's audio state machine. No sample assets are required.
"""
from pathlib import Path
import os, re, subprocess, tempfile
root = Path(__file__).resolve().parents[1]
source = (root / 'sfx.c').read_text()
source = re.sub(r'^#include .*\n', '', source, flags=re.M)
source = re.sub(r'^INCBIN_CHIP.*\n', '', source, flags=re.M)
source = source.replace('__attribute__((section(".MEMF_CHIP"),aligned(2)))', '')
a = source.index('static void settle_dma(void) {')
b = source.index('static void play(', a)
source = source[:a] + 'static void settle_dma(void) { dma_boundary(); }\n' + source[b:]
header = r'''
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
typedef uint16_t UWORD; typedef uint8_t UBYTE; typedef uintptr_t ULONG;
#define INTF_AUD0 0x80
#define INTF_SETCLR 0x8000
#define INTF_INTEN 0x4000
#define DMAF_SETCLR 0x8000
#define DMAF_MASTER 0x200
#define SFX_COUNT 16
#define SFX_BIT(id) (1u<<(id))
static UBYTE SfxData[128];
static const struct {unsigned offset; unsigned short words;} sfx_samples[16]={
 {0,2},{4,2},{8,2},{12,2},{16,2},{20,2},{24,2},{28,2},
 {32,2},{36,2},{40,2},{44,0},{48,2},{52,2},{56,2},{60,2}};
static struct {UWORD sfx_period;} video_timing={443};
static struct {
 UWORD intena,dmacon,intreq,adkcon;
 struct {ULONG ac_ptr; UWORD ac_len,ac_per,ac_vol;} aud[4];
} registers, *custom=&registers;
static void (*callbacks[4])(unsigned);
static void paula_irq_set(unsigned ch,void (*fn)(unsigned)) {callbacks[ch]=fn;}
static unsigned phase,expected_id,selected_ch,early_irq,starts;
static void dma_boundary(void);
'''
footer = r'''
static void dma_boundary(void) {
 unsigned ch=0;
 for(unsigned i=1;i<4;++i) if(custom->dmacon & (1u<<i)) {assert(!ch);ch=i;}
 assert(ch);
 assert(custom->aud[ch].ac_vol==0); // also while new DMA is fetching
 if(!phase) {
  selected_ch=ch;
  assert(custom->intena==(INTF_AUD0<<ch));
  custom->intreq=0xdead; // a late request, AFTER stop() has cleared INTREQ
  phase=1;
 } else {
  assert(ch==selected_ch);
  assert(custom->intreq==(INTF_AUD0<<ch)); // stale request cleared before enable
  assert(custom->dmacon==(DMAF_SETCLR|DMAF_MASTER|(1u<<ch)));
  assert(custom->aud[ch].ac_ptr==(ULONG)(SfxData+sfx_samples[expected_id].offset));
  assert(custom->aud[ch].ac_len==2);
  assert(custom->aud[ch].ac_per==video_timing.sfx_period);
  if(early_irq) callbacks[ch](ch);
  assert(custom->aud[ch].ac_vol==0);
  phase=0;++starts;
 }
}
static void request(unsigned id) {
 expected_id=id;sfx_emit(SFX_BIT(id));
 assert(!phase);
 assert(custom->aud[selected_ch].ac_vol==64);
 assert(custom->aud[selected_ch].ac_ptr==(ULONG)silence);
 assert(custom->aud[selected_ch].ac_len==1);
}
static void retire_all(void) {
 for(unsigned ch=1;ch<4;++ch) if(busy[ch]) {
  if(first[ch]) callbacks[ch](ch);
  callbacks[ch](ch);
  assert(!busy[ch] && !custom->aud[ch].ac_vol);
 }
}
int main(void) {
 sfx_init();assert(!callbacks[0]);
 for(unsigned video=0;video<2;++video) {
  video_timing.sfx_period=video?447:443;
  for(early_irq=0;early_irq<2;++early_irq) {
   for(unsigned id=0;id<SFX_COUNT;++id) {
    if(id==11) {unsigned n=starts;sfx_emit(SFX_BIT(id));assert(starts==n);continue;}
    request(id);retire_all();
   }
   request(0);unsigned same=selected_ch;
   request(0);assert(selected_ch==same); // retrigger
   request(1);assert(selected_ch!=same);
   request(2);assert(selected_ch!=same);
   request(3);assert(selected_ch==same); // oldest voice is stolen
   retire_all();
  }
 }
 unsigned b,r,a;sfx_status(&b,&r,&a);assert(b==starts && r==starts && a==0);
 sfx_shutdown();for(unsigned ch=1;ch<4;++ch) assert(!callbacks[ch]);
 puts("SFX startup: muted fetch, stale IRQ clearance, PAL/NTSC, early/late IRQ, retrigger and voice stealing passed");
}
'''
with tempfile.TemporaryDirectory() as tmp:
    path = Path(tmp)
    (path/'check.c').write_text(header+source+footer)
    subprocess.run([os.environ.get('HOST_CC','cc'), '-std=c99','-O2','-Wall','-Wextra','-Werror',str(path/'check.c'),'-o',str(path/'check')], check=True)
    subprocess.run([str(path/'check')], check=True, timeout=10)
