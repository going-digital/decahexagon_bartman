/* Predecoded PCM DMA transport: no runtime decompression or interpolation. */
#include "../system.h"
#include "../config.h"
#include "fib_stream.h"
#include "../fib_song.h"
#include "../pcm_lifecycle.h"
#include "../paula_irq.h"
#include "../sfx.h"
#include "../pc_pulse.h"
INCBIN(CourtesyCues, "assets/music1.cues");
static PcmLifecycle lifecycle;
INCBIN(FibSongData, PCM_BANK_FIRST);
INCBIN_CHIP(FibSongTail, PCM_BANK_SECOND);
static FibSong song;
#define FIB_BUFFERS 4
static UBYTE *buffers;
static volatile UWORD state[FIB_BUFFERS]; /* 0 free, 1 decoded, 2 owned by DMA */
static volatile UWORD underruns, blocks;
static UWORD playing, pending, next_queue, first_irq;
static unsigned fill_slot;
static volatile UWORD running;
static UWORD started, rendered, display_frames;
static ULONG sample_length, fill_position, positions[FIB_BUFFERS];
static volatile ULONG audible_position;
static volatile UWORD audio_frame, audio_line, audio_epoch;

/* VBlank can be pending when the higher-priority audio IRQ runs. Include it
 * in the timestamp without changing the system's frame counter. */
static void beam_stamp(UWORD *frame,UWORD *line) {
    UWORD before,after,pending;
    do {
        before=(UWORD)frameCounter;
        pending=custom->intreqr&INTF_VERTB;
        *line=(*(volatile ULONG*)&custom->vposr>>8)&511;
        after=(UWORD)frameCounter;
    } while(before!=after || pending!=(custom->intreqr&INTF_VERTB));
    *frame=after+(pending?1:0);
}
unsigned fib_stream_cue(void) {
    if(!running) return 0;
    UWORD epoch,frame,line,af,al;
    ULONG position;
    do {
        epoch=audio_epoch;
        position=audible_position;af=audio_frame;al=audio_line;
        beam_stamp(&frame,&line);
    } while(epoch!=audio_epoch);
    int lines=(UWORD)(frame-af)*video_timing.lines+(int)line-al;
    if(lines<0) lines=0;
    position=pc_pcm_position(position,sample_length,(unsigned)lines,video_timing.music_period);
    unsigned index=pc_pulse_cue_index(position);
    return index<11441?((const UBYTE*)CourtesyCues)[index]:0;
}
void fib_stream_frame(unsigned elapsed) {++rendered;display_frames+=elapsed;}

static void queue(unsigned slot) {
    *(volatile ULONG *)&custom->aud[0].ac_ptr=(ULONG)(buffers+slot*512);
    custom->aud[0].ac_len=256;
}
static void audio_irq(unsigned channel) {
    (void)channel;
    custom->intreq=INTF_AUD0;
    custom->intreq=INTF_AUD0;
    if(first_irq) first_irq=0; /* initial DMA fetch has started buffer zero */
    else {
        if(playing!=pending) state[playing]=0;
        playing=pending;
        ++blocks;
    }
    UWORD frame,line;beam_stamp(&frame,&line);
    audible_position=positions[playing];audio_frame=frame;audio_line=line;
    ++audio_epoch;
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
static void fill_one(void) {
    positions[fill_slot]=fill_position;
    fib_song_read(&song,buffers+fill_slot*512,512);
    fill_position+=512;
    if(fill_position>=sample_length) fill_position-=sample_length;
    __asm volatile ("" ::: "memory");
    state[fill_slot]=1;
    fill_slot=(fill_slot+1)%FIB_BUFFERS;
}
void fib_stream_start(void) {
    underruns=blocks=0;
    unsigned bytes=(ULONG)&incbin_FibSongData_end-(ULONG)FibSongData;
    if(!fib_pcm_init_split(&song,FibSongData,bytes,FibSongTail,
        (ULONG)&incbin_FibSongTail_end-(ULONG)FibSongTail)) {
        underruns=999; return;
    }
    buffers=AllocMem(512*FIB_BUFFERS,MEMF_CHIP);
    if(!buffers) { underruns=999; return; }
    const UBYTE *header=(const UBYTE*)FibSongData;
    sample_length=((ULONG)header[4]<<24)|((ULONG)header[5]<<16)|((ULONG)header[6]<<8)|header[7];
    fill_position=fill_slot=0;
    for(unsigned i=0;i<FIB_BUFFERS;++i) fill_one();
    playing=pending=0;next_queue=1;first_irq=1;state[0]=2;
    custom->intena=INTF_AUD0;
    custom->dmacon=DMAF_AUD0;
    paula_irq_set(0,audio_irq);
    custom->adkcon=0x00ff; /* no volume/period attachment */
    queue(0);
    custom->aud[0].ac_per=video_timing.music_period;
    custom->aud[0].ac_vol=0;
    custom->intreq=INTF_AUD0;
    custom->intreq=INTF_AUD0;
    started=(UWORD)frameCounter;
    UWORD frame,line;beam_stamp(&frame,&line);
    audible_position=0;audio_frame=frame;audio_line=line;++audio_epoch;
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
    /* One PCM copy per VBlank. Level-4 audio IRQ can preempt safely. */
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
    paula_irq_set(0,0);
    FreeMem(buffers,512*FIB_BUFFERS);
    buffers=0;
}
void fib_stream_tick(unsigned playing,unsigned menu) {
    unsigned action=pcm_lifecycle_tick(&lifecycle,playing,menu);
    if(action&PCM_STOP) fib_stream_stop();
    if(action&PCM_START) fib_stream_start();
    if(running) custom->aud[0].ac_vol=lifecycle.volume;
}
void fib_stream_draw(unsigned char *plane) {
    static const UBYTE glyphs[14][5]={
      {6,9,9,9,6},{2,6,2,2,7},{14,1,6,8,15},{14,1,6,1,14},
      {9,9,15,1,1},{15,8,14,1,14},{7,8,14,9,6},{15,1,2,4,4},
      {6,9,6,9,6},{6,9,7,1,14},{9,9,9,9,6},{14,9,14,9,14},{14,9,14,10,9},{9,9,9,6,6}};
    unsigned u=underruns,b=blocks;
    unsigned r=rendered,v=display_frames;
    unsigned sb,sr,sa;sfx_status(&sb,&sr,&sa);
    unsigned text[32]={10,u/100,(u/10)%10,u%10,11,b/10000,(b/1000)%10,(b/100)%10,(b/10)%10,b%10,12,r/10000,(r/1000)%10,(r/100)%10,(r/10)%10,r%10,13,v/10000,(v/1000)%10,(v/100)%10,(v/10)%10,v%10,11,sb/100,(sb/10)%10,sb%10,12,sr/100,(sr/10)%10,sr%10,13,sa};
    /* Byte-aligned glyphs keep diagnostics from dominating render time. */
    static const UBYTE expanded[16]={0,3,12,15,48,51,60,63,192,195,204,207,240,243,252,255};
    for(unsigned ch=0;ch<32;++ch) {
        unsigned column=ch<10?ch:ch<22?ch-10:ch-22;
        unsigned gap=ch<10?4:ch<22?6:4;
        unsigned x=1+column+(column>=gap);
        unsigned top=ch<10?26:ch<22?40:54;
        for(unsigned y=0;y<5;++y) {
            UBYTE bits=expanded[glyphs[text[ch]][y]];
            plane[(top+2*y)*SCREEN_WIDTH_BYTES+x]=bits;
            plane[(top+2*y+1)*SCREEN_WIDTH_BYTES+x]=bits;
            plane[(top+2*y)*SCREEN_WIDTH_BYTES+1+gap]=0;
            plane[(top+2*y+1)*SCREEN_WIDTH_BYTES+1+gap]=0;
        }
    }
}
