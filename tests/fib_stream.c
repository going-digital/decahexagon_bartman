/* Experimental DMA transport; optional complete FBS1 bank sequencer. */
#include "../system.h"
#include "../config.h"
#include "../fib_decode.h"
#include "fib_stream.h"
#if FIB_TRIAL_SONG
#include "../fib_song.h"
INCBIN(FibSongData, "out/courtesy.fbs");
static FibSong song;
#else
INCBIN(FibTrial, "out/fib_trial.payload");
#endif
#if FIB_TRIAL_SONG
#define FIB_BUFFERS 4
#else
#define FIB_BUFFERS 3
#endif
static UBYTE *buffers;
static volatile UWORD state[FIB_BUFFERS]; /* 0 free, 1 decoded, 2 owned by DMA */
static volatile UWORD underruns, blocks;
static UWORD playing, pending, next_queue, first_irq;
static unsigned fill_slot, source_block, source_blocks, fill_position;
static volatile UWORD running;
static UWORD started, rendered, display_frames;
void fib_stream_frame(unsigned elapsed) {++rendered;display_frames+=elapsed;}
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
        next_queue=(next_queue+1)%FIB_BUFFERS;
    } else {
        /* Repeat the current immutable buffer; never DMA partially decoded data. */
        pending=playing;
        if(underruns<999) ++underruns;
    }
    queue(pending); /* registers describe the following hardware reload */
}
static void fill_one(unsigned samples) {
#if FIB_TRIAL_SONG
    fib_song_read(&song,buffers+fill_slot*512+fill_position,samples);
    fill_position+=samples;
    if(fill_position<512) return;
    fill_position=0;
#else
    fib_decode_block((const UBYTE*)FibTrial+source_block*257,buffers+fill_slot*512,512);
    source_block=(source_block+1)%source_blocks;
#endif
    __asm volatile ("" ::: "memory");
    state[fill_slot]=1;
    fill_slot=(fill_slot+1)%FIB_BUFFERS;
}
void fib_stream_start(void) {
#if FIB_TRIAL_SONG
    if(!fib_song_init(&song,FibSongData,(ULONG)&incbin_FibSongData_end-(ULONG)FibSongData)) {
        underruns=999; return;
    }
#else
    unsigned bytes=(ULONG)&incbin_FibTrial_end-(ULONG)FibTrial;
    if(!bytes || bytes%257) { underruns=999; return; }
    source_blocks=bytes/257;
#endif
    buffers=AllocMem(512*FIB_BUFFERS,MEMF_CHIP);
    if(!buffers) { underruns=999; return; }
    fill_slot=source_block=fill_position=0;
    for(unsigned i=0;i<FIB_BUFFERS;++i) fill_one(512);
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
    __asm volatile ("" ::: "memory");
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
    if(state[fill_slot]==0) {
#if FIB_TRIAL_SONG
        /* Cheap spans can safely refill a whole block in one tick. Never
         * let this fast path cross into a potentially interpolated segment. */
        unsigned remaining=512-fill_position;
        unsigned quota=(song.output_left>=remaining &&
            (song.raw_mode || song.source_length==song.target_length))?remaining:256;
        fill_one(quota);
#else
        fill_one(512);
#endif
    }
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
    FreeMem(buffers,512*FIB_BUFFERS);
    buffers=0;
}
void fib_stream_draw(unsigned char *plane) {
    static const UBYTE glyphs[14][5]={
      {6,9,9,9,6},{2,6,2,2,7},{14,1,6,8,15},{14,1,6,1,14},
      {9,9,15,1,1},{15,8,14,1,14},{7,8,14,9,6},{15,1,2,4,4},
      {6,9,6,9,6},{6,9,7,1,14},{9,9,9,9,6},{14,9,14,9,14},{14,9,14,10,9},{9,9,9,6,6}};
    unsigned u=underruns,b=blocks;
    unsigned r=rendered,v=display_frames;
    unsigned text[22]={10,u/100,(u/10)%10,u%10,11,b/10000,(b/1000)%10,(b/100)%10,(b/10)%10,b%10,12,r/10000,(r/1000)%10,(r/100)%10,(r/10)%10,r%10,13,v/10000,(v/1000)%10,(v/100)%10,(v/10)%10,v%10};
    /* Byte-aligned glyphs keep diagnostics from dominating render time. */
    static const UBYTE expanded[16]={0,3,12,15,48,51,60,63,192,195,204,207,240,243,252,255};
    for(unsigned ch=0;ch<22;++ch) {
        unsigned column=ch<10?ch:ch-10;
        unsigned gap=ch<10?4:6;
        unsigned x=1+column+(column>=gap);
        unsigned top=ch<10?26:40;
        for(unsigned y=0;y<5;++y) {
            UBYTE bits=expanded[glyphs[text[ch]][y]];
            plane[(top+2*y)*SCREEN_WIDTH_BYTES+x]=bits;
            plane[(top+2*y+1)*SCREEN_WIDTH_BYTES+x]=bits;
            plane[(top+2*y)*SCREEN_WIDTH_BYTES+1+gap]=0;
            plane[(top+2*y+1)*SCREEN_WIDTH_BYTES+1+gap]=0;
        }
    }
}
