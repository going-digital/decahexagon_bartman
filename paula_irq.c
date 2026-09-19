#include "system.h"
#include "paula_irq.h"
static PaulaCallback callbacks[4];
static APTR saved;
static APTR *vector;
__attribute__((interrupt)) static void audio_interrupt(void) {
    unsigned pending=custom->intreqr & custom->intenar & 0x0780;
    for(unsigned ch=0;ch<4;++ch) {
        unsigned mask=INTF_AUD0<<ch;
        if(pending&mask) {
            custom->intreq=mask;custom->intreq=mask;
            if(callbacks[ch]) callbacks[ch](ch);
        }
    }
}
void paula_irq_init(void) {
    custom->intena=0x0780;custom->intreq=0x0780;custom->intreq=0x0780;
    vector=(APTR*)((UBYTE*)GetSystemVBR()+0x70);saved=*vector;
    *vector=(APTR)audio_interrupt;
}
void paula_irq_set(unsigned ch,PaulaCallback callback) {
    /* Caller has masked this channel before changing its callback. */
    callbacks[ch]=callback;
    __asm volatile("" ::: "memory");
}
void paula_irq_shutdown(void) {
    custom->intena=0x0780;custom->dmacon=0x000f;
    custom->intreq=0x0780;custom->intreq=0x0780;
    for(unsigned ch=0;ch<4;++ch) {custom->aud[ch].ac_vol=0;callbacks[ch]=0;}
    *vector=saved;
}
