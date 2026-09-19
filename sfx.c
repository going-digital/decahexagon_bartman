#include "system.h"
#include "sfx.h"
#include "paula_irq.h"
#include "out/sfx_samples.h"
INCBIN_CHIP(SfxData,"out/sfx.pcm");
static const UWORD silence[2] __attribute__((section(".MEMF_CHIP"),aligned(2)))={0,0};
static volatile UWORD busy[4], first[4];
static volatile unsigned begun,retired;
static unsigned serial, age[4], clip_id[4];
static void stop(unsigned ch) {
    unsigned mask=INTF_AUD0<<ch;
    custom->intena=mask;custom->dmacon=1u<<ch;
    custom->aud[ch].ac_vol=0;
    custom->intreq=mask;custom->intreq=mask;
    if(busy[ch]) ++retired;
    busy[ch]=0;
}
static void done(unsigned ch) {
    if(first[ch]) {
        first[ch]=0;
        /* Following reload is silence, so interrupt latency cannot repeat
         * the clip. The next IRQ stops DMA once that reload begins. */
        *(volatile ULONG*)&custom->aud[ch].ac_ptr=(ULONG)silence;
        custom->aud[ch].ac_len=1;
    } else stop(ch);
}
void sfx_init(void) {
    for(unsigned ch=1;ch<4;++ch) {stop(ch);paula_irq_set(ch,done);}
    custom->adkcon=0x00ff;
}
static void settle_dma(void) {
    for(unsigned n=0;n<2;++n) {
        UWORD line=custom->vhposr&0xff00;
        while((custom->vhposr&0xff00)==line) {}
    }
}
static void play(unsigned id) {
    if(!sfx_samples[id].words) return; /* omitted, currently unused clip */
    unsigned ch=0;
    /* Replaying the same effect retriggers it, as PC playef does. Otherwise
     * use a free voice, then steal the oldest of the three hardware voices. */
    for(unsigned i=1;i<4;++i) if(busy[i] && clip_id[i]==id) {ch=i;break;}
    if(!ch) for(unsigned i=1;i<4;++i) if(!busy[i]) {ch=i;break;}
    if(!ch) {ch=1;for(unsigned i=2;i<4;++i) if(age[i]<age[ch]) ch=i;}
    stop(ch);
    /* Allow a prior DMA request to settle before programming a new sample. */
    settle_dma();
    *(volatile ULONG*)&custom->aud[ch].ac_ptr=(ULONG)((const UBYTE*)SfxData+sfx_samples[id].offset);
    custom->aud[ch].ac_len=sfx_samples[id].words;
    custom->aud[ch].ac_per=video_timing.sfx_period;
    custom->aud[ch].ac_vol=64;
    ++begun;age[ch]=++serial;clip_id[ch]=id;first[ch]=busy[ch]=1;
    __asm volatile("" ::: "memory");
    custom->intena=INTF_SETCLR|INTF_INTEN|(INTF_AUD0<<ch);
    custom->dmacon=DMAF_SETCLR|DMAF_MASTER|(1u<<ch);
    /* Preload silence after the first fetch independently of IRQ timing.
     * An initial request can be delayed/coalesced; relying on its callback
     * alone can leave the original clip queued for a second playback. */
    settle_dma();
    *(volatile ULONG*)&custom->aud[ch].ac_ptr=(ULONG)silence;
    custom->aud[ch].ac_len=1;
}
void sfx_emit(uint16_t mask) {
    for(unsigned id=0;id<SFX_COUNT;++id) if(mask&SFX_BIT(id)) play(id);
}
void sfx_shutdown(void) {for(unsigned ch=1;ch<4;++ch) {stop(ch);paula_irq_set(ch,0);}}

void sfx_status(unsigned *b,unsigned *r,unsigned *a) {
    *b=begun%1000;*r=retired%1000;*a=0;
    for(unsigned ch=1;ch<4;++ch) if(busy[ch]) *a|=1u<<(ch-1);
}
