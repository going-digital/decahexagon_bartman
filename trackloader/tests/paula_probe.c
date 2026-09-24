/* Loading diagnostic only: poll AUD0 while interrupts are masked. The game
 * keeps its IRQ-driven player; this isolates real DMA and decoded PCM delivery. */
#include "../../fib_pcm.h"
#include "../../trackloader/tune.h"
#define REG16(offset) (*(volatile unsigned short *)(0xdff000+(offset)))
static void queue(unsigned char *p) {
    *(volatile unsigned long *)0xdff0a0=(unsigned long)p;
    REG16(0xa4)=256;
}
static int wait_audio(void) {
    unsigned budget=2000000;
    while(!(REG16(0x1e)&0x80)) if(!--budget)return 0;
    REG16(0x9c)=0x80;REG16(0x9c)=0x80;
    return 1;
}
unsigned trackloader_paula_probe(PcmSong *song,TrackTuneInfo *info,unsigned char *dma) {
    if(!fib_song_seek(song,0))return 0;
    fib_song_read(song,dma,512);fib_song_read(song,dma+512,512);
    REG16(0x9a)=0x80;REG16(0x96)=1;REG16(0x9e)=0x00ff;
    REG16(0x9c)=0x80;REG16(0x9c)=0x80;
    queue(dma);REG16(0xa6)=296;REG16(0xa8)=64;REG16(0x96)=0x8201;
    if(!wait_audio())goto fail;
    queue(dma+512);
    for(unsigned i=0;i<128;i++) {
        if(!wait_audio())goto fail;
        unsigned char *free=dma+(i&1)*512;
        fib_song_read(song,free,512);queue(free);
    }
    REG16(0x96)=1;REG16(0xa8)=0;REG16(0x9c)=0x80;return 1;
fail:
    REG16(0x96)=1;REG16(0xa8)=0;return 0;
}
