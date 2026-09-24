/* A500-only diagnostic adapter for the actual game's external PCM IRQ player. */
#include "../../system.h"
#include "../../config.h"
#include "../../tests/fib_stream.h"
#include "../../paula_irq.h"
#include "../../trackloader/tune.h"
struct Custom *custom=(struct Custom *)0xdff000;
volatile short frameCounter;
APTR GetSystemVBR(void) { return 0; }
__attribute__((interrupt)) static void probe_vblank(void) {
    custom->intreq=INTF_VERTB;custom->intreq=INTF_VERTB;
    frameCounter++;
    fib_stream_fill();
}
unsigned trackloader_paula_probe(PcmSong *song,TrackTuneInfo *info,unsigned char *dma) {
    video_select(1);
    *(APTR volatile *)0x6c=(APTR)probe_vblank;
    paula_irq_init();
    unsigned result=1;
    frameCounter=0;
    custom->intreq=INTF_VERTB;
    custom->intena=INTF_SETCLR|INTF_INTEN|INTF_VERTB;
    __asm volatile("move.w #0x2000,%%sr" : : : "memory","cc");
    for(unsigned pass=0;pass<2 && result;pass++) {
        /* Overwrite retired DMA storage to expose stale-buffer playback. */
        for(unsigned i=0;i<2048;i++)dma[i]=0x55;
        if(!fib_stream_bind(song,info->samples,0,0,dma)){result=0;break;}
        unsigned start=(UWORD)frameCounter,previous=start,accumulator=0;
        fib_stream_tick(1,0);
        if(fib_stream_bind(song,info->samples,0,0,dma) || fib_stream_unbind())result=0;
        while((UWORD)((UWORD)frameCounter-start)<150) {
            unsigned now=(UWORD)frameCounter;
            if(now!=previous) {
                accumulator+=(UWORD)(now-previous)*60;previous=now;
                while(accumulator>=50) {accumulator-=50;fib_stream_tick(1,0);}
            }
        }
        fib_stream_stop();
        unsigned underruns,blocks;fib_stream_status(&underruns,&blocks);
        if(underruns || blocks<65 || !fib_stream_unbind())result=0;
        if(custom->dmaconr&DMAF_AUD0)result=0;
        unsigned stopped=(UWORD)frameCounter;
        while((UWORD)((UWORD)frameCounter-stopped)<50) {}
        unsigned idle_u,idle_b;fib_stream_status(&idle_u,&idle_b);
        if(idle_u!=underruns || idle_b!=blocks)result=0;
    }
    __asm volatile("move.w #0x2700,%%sr" : : : "memory","cc");
    custom->intena=INTF_VERTB;custom->intreq=INTF_VERTB;
    paula_irq_shutdown();
    return result;
}
