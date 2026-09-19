/* Experimental continuous-loop transport, not the beat-bank sequencer. */
#include "../system.h"
#include "../config.h"
#include "../fib_decode.h"
#include "fib_stream.h"
INCBIN(FibTrial, "out/fib_trial.payload");
static UBYTE *buffers;
static volatile UWORD state[3]; /* 0 free, 1 decoded, 2 owned by DMA */
static volatile UWORD underruns, blocks;
static UWORD playing, pending, next_queue, first_irq;
static unsigned fill_slot, source_block, source_blocks;
static volatile UWORD running;
static UWORD started;
static APTR saved_vector;
static APTR *vector;

static void queue(unsigned slot) {
    *(volatile ULONG *)&custom->aud[0].ac_ptr=(ULONG)(buffers+slot*512);
    custom->aud[0].ac_len=256;
}
__attribute__((interrupt)) static void audio_irq(void) {
    custom->intreq=INTF_AUD0;
    custom->intreq=INTF_AUD0;
    if(first_irq) first_irq=0; /* initial DMA fetch has started buffer zero */
    else {
        if(playing!=pending) state[playing]=0;
        playing=pending;
        ++blocks;
    }
    if(state[next_queue]==1) {
        pending=next_queue;
        state[pending]=2;
        next_queue=(next_queue+1)%3;
    } else {
        /* Repeat the current immutable buffer; never DMA partially decoded data. */
        pending=playing;
        if(underruns<999) ++underruns;
    }
    queue(pending); /* registers describe the following hardware reload */
}
static void fill_one(void) {
    fib_decode_block((const UBYTE*)FibTrial+source_block*257,buffers+fill_slot*512,512);
    source_block=(source_block+1)%source_blocks;
    __asm volatile ("" ::: "memory");
    state[fill_slot]=1;
    fill_slot=(fill_slot+1)%3;
}
void fib_stream_start(void) {
    unsigned bytes=(ULONG)&incbin_FibTrial_end-(ULONG)FibTrial;
    if(!bytes || bytes%257) { underruns=999; return; }
    source_blocks=bytes/257;
    buffers=AllocMem(1536,MEMF_CHIP);
    if(!buffers) { underruns=999; return; }
    fill_slot=source_block=0;
    for(unsigned i=0;i<3;++i) fill_one();
    playing=pending=0;next_queue=1;first_irq=1;state[0]=2;
    custom->intena=INTF_AUD0;
    custom->dmacon=DMAF_AUD0;
    vector=(APTR*)((UBYTE*)GetSystemVBR()+0x70);
    saved_vector=*vector;
    *vector=(APTR)audio_irq;
    custom->adkcon=0x00ff; /* no volume/period attachment */
    queue(0);
#ifdef TARGET_NTSC
    custom->aud[0].ac_per=298; /* 3579545 / 298 = 12011.90 Hz */
#else
    custom->aud[0].ac_per=296; /* 3546895 / 296 = 11982.75 Hz */
#endif
    custom->aud[0].ac_vol=64;
    custom->intreq=INTF_AUD0;
    custom->intreq=INTF_AUD0;
    started=(UWORD)frameCounter;
    running=1;
    custom->intena=INTF_SETCLR|INTF_INTEN|INTF_AUD0;
    custom->dmacon=DMAF_SETCLR|DMAF_MASTER|DMAF_AUD0;
}
void fib_stream_fill(void) {
    if(!running) return;
#if FIB_TRIAL_STALL
    UWORD age=(UWORD)((UWORD)frameCounter-started);
    if(age>=250 && age<265) return; /* deliberate producer starvation */
#endif
    /* One decode per VBlank (or per frame in the negative baseline build).
     * Level-4 audio IRQ can preempt this level-3 producer safely. */
    if(state[fill_slot]==0) fill_one();
}
void fib_stream_stop(void) {
    running=0;
    __asm volatile ("" ::: "memory");
    if(!buffers) return;
    custom->intena=INTF_AUD0;
    custom->dmacon=DMAF_AUD0;
    custom->aud[0].ac_vol=0;
    custom->intreq=INTF_AUD0;
    custom->intreq=INTF_AUD0;
    *vector=saved_vector;
    FreeMem(buffers,1536);
    buffers=0;
}
void fib_stream_draw(unsigned char *plane) {
    static const UBYTE glyphs[12][5]={
      {6,9,9,9,6},{2,6,2,2,7},{14,1,6,8,15},{14,1,6,1,14},
      {9,9,15,1,1},{15,8,14,1,14},{7,8,14,9,6},{15,1,2,4,4},
      {6,9,6,9,6},{6,9,7,1,14},{9,9,9,9,6},{14,9,14,9,14}};
    unsigned u=underruns,b=blocks;
    unsigned text[10]={10,u/100,(u/10)%10,u%10,11,b/10000,(b/1000)%10,(b/100)%10,(b/10)%10,b%10};
    for(unsigned ch=0;ch<10;++ch)
      for(unsigned y=0;y<10;++y)
        for(unsigned x=0;x<8;++x) {
          unsigned px=8+ch*10+x;
          UBYTE *p=plane+(26+y)*SCREEN_WIDTH_BYTES+(px>>3), mask=0x80>>(px&7);
          if(glyphs[text[ch]][y/2]&(8>>(x/2))) *p|=mask; else *p&=~mask;
        }
}
