#include "native_save.h"
#include "save_disk.h"
#include "../system.h"
#include "../blitter.h"
#include "../coplist.h"
#include "../hud.h"
#include "../sfx.h"
#include "../tests/fib_stream.h"
/* The resident supplies dedicated Chip RAM scratch, separate from its inflater. */
_Static_assert(sizeof(TrackSaveDiskScratch)==2048,"resident save workspace size");
#if WHDLOAD
static int (*commit_save)(const TrackSave *);
#else
static unsigned char drive_select;
static TrackSaveMedia media;
static TrackSaveDiskScratch *scratch;
#endif
static TrackSave pending;
#if WHDLOAD
static unsigned pending_valid,attempted;
#else
static int (*transfer)(unsigned,unsigned,void*);
static unsigned pending_valid,attempted;
static uint32_t be32(const unsigned char *p) {
    return ((uint32_t)p[0]<<24)|((uint32_t)p[1]<<16)|((uint32_t)p[2]<<8)|p[3];
}
#endif
void native_save_init(const TrackGameBoot *boot) {
#if WHDLOAD
    pending_valid=attempted=0;commit_save=boot->commit_save;
#else
    drive_select=(unsigned char)~(1u<<(3+(boot->boot_drive&3)));
    transfer=boot->boot_drive<4?boot->disk_transfer:0;pending_valid=attempted=0;
    scratch=(TrackSaveDiskScratch*)boot->save_scratch;
    if(!scratch || ((ULONG)scratch&1) || !transfer || !boot->save_identity || !boot->save_anchors){transfer=0;return;}
    for(unsigned i=0;i<16;i++)media.identity[i]=boot->save_identity[i];
    for(unsigned i=0;i<3;i++) {
        media.sector[i]=be32(boot->save_anchors+4*i);
        media.expected[i]=boot->save_anchors+12+512*i;
    }
#endif
}
#if !WHDLOAD
static int disk_write_protected(void) {
    unsigned short sr;
    volatile unsigned char *port=(volatile unsigned char*)0xbfd100;
    volatile unsigned char *sense=(volatile unsigned char*)0xbfe001;
    __asm volatile("move.w %%sr,%0\n move.w #0x2700,%%sr":"=d"(sr)::"memory","cc");
    unsigned char previous=*port;
    *port=drive_select; /* Boot drive selected, motor off. */
    (void)*sense; /* CIA bus cycle for selection to settle. */
    int protected= !(*sense & 8); /* active-low /DSKPROT */
    *port=previous;
    __asm volatile("move.w %0,%%sr"::"d"(sr):"memory","cc");
    return protected;
}
static int read_sector(void *ctx,uint32_t n,unsigned char *b) {(void)ctx;return transfer(0,n,b);}
static int write_sector(void *ctx,uint32_t n,const unsigned char *b) {(void)ctx;return transfer(1,n,(void*)b);}
#endif
int native_save_tick(void *plane) {
    if(game_mode()==MODE_PLAYING){attempted=0;hud_save_failed(0);return 0;}
#if WHDLOAD
    if(!commit_save || attempted
#else
    if(!transfer || attempted
#endif
       /* Keep the death/retry loop uninterrupted. Persist accumulated runs
        * only when the player deliberately returns to level selection. */
       || game_mode()!=MODE_ATTRACT)return 0;
    if(!pending_valid)pending_valid=game_save_snapshot(&pending);
    if(!pending_valid)return 0;
    attempted=1;
#if !WHDLOAD
    /* Query the selected drive before stopping audio or doing any disk I/O. */
    if(disk_write_protected()) { hud_save_failed(2);return 0; }
#endif
    blit_wait();WaitDisplayList();
    unsigned short sr;
    __asm volatile("move.w %%sr,%0\n move.w #0x2700,%%sr":"=d"(sr)::"memory","cc");
    fib_stream_stop();sfx_shutdown();
    UWORD dma=custom->dmaconr&0x07f0,interrupts=custom->intenar&0x7fff;
    custom->intena=0x7fff;custom->dmacon=0x7fff;
    custom->cop1lc=(ULONG)hud_loading_copper(plane,1);custom->copjmp1=0;
    custom->dmacon=0x83a0;
    hud_loading_begin();
    custom->intena=INTF_SETCLR|INTF_INTEN|INTF_VERTB;
    __asm volatile("move.w #0x2200,%%sr":::"memory","cc");
#if WHDLOAD
    int status=commit_save(&pending);
#else
    transfer(2,0,0); /* begin a new uncached media-check sequence */
    int status=track_save_disk_commit(read_sector,write_sector,0,&media,&pending,scratch);
#endif
    __asm volatile("move.w #0x2700,%%sr":::"memory","cc");
    hud_loading_end();
    if(status==TRACK_SAVE_DISK_OK) {
        game_save_committed(&pending);pending_valid=0;attempted=!game_save_dirty();
    }
    hud_save_failed(status!=TRACK_SAVE_DISK_OK);
    sfx_init();custom->dmacon=0x7fff;
    custom->cop1lc=(ULONG)copper_dispatch;custom->copjmp1=0;
    custom->intreq=0x7fff;custom->dmacon=0x8000|dma;custom->intena=0x8000|interrupts;
    __asm volatile("move.w %0,%%sr"::"d"(sr):"memory","cc");
    return 1;
}

#if WHDLOAD
int native_save_finish(void *plane) {
    attempted=0;native_save_tick(plane);
    return !game_save_dirty();
}
#endif
