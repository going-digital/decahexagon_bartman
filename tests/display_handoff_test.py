#!/usr/bin/env python3
"""Exercise the real display publisher/IRQ with interrupts during list writes.

Mock registers check publication and acknowledgement, not Copper bus timing.
"""
import os
from pathlib import Path
import subprocess
import tempfile

root = Path(__file__).resolve().parents[1]
source = (root / 'system.c').read_text()
queue = source[source.index('static APTR volatile pending_display;'):
               source.rfind('#if !TRACKLOADER', 0, source.index('static struct View *ActiView;'))]
irq = source[source.index('__attribute__((interrupt)) void interruptHandler'):]
irq = irq.replace('__attribute__((interrupt)) ', '')
header = r'''
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
typedef void *APTR;
typedef uint16_t UWORD;
typedef uintptr_t ULONG;
#define INTF_VERTB 0x20
#define DMAF_BLITTER 0x40
#define DMAF_SETCLR 0x8000
static struct {
    UWORD intreq, dmaconr, dmacon, copjmp1;
    ULONG vposr, cop2lc;
} registers, *custom = &registers;
static unsigned frameCounter;
'''
test = r'''
int main(void) {
    UWORD lists[2][128] = {{0}};
    unsigned active = 0;
    custom->cop2lc = (ULONG)lists[active];
    for (unsigned frame = 1; frame <= 1000; ++frame) {
        unsigned draw = active ^ 1;
        /* Force VBlanks throughout construction, including between the
         * two words that would encode a bitplane pointer. */
        for (unsigned word = 0; word < 128; ++word) {
            lists[draw][word] = frame;
            custom->vposr = 0;
            interruptHandler();
            assert(custom->cop2lc == (ULONG)lists[active]);
            for (unsigned i = 0; i < 128; ++i)
                assert(lists[active][i] == frame - 1);
        }
        QueueDisplayList(lists[draw]);
        /* Late IRQs on either video standard must leave ownership pending. */
        const unsigned late[] = {8, 15, 16, 44, 100, 255, 261, 311};
        for (unsigned i = 0; i < sizeof(late)/sizeof(late[0]); ++i) {
            custom->vposr = late[i] << 8;
            interruptHandler();
            assert(pending_display == lists[draw]);
            assert(custom->cop2lc == (ULONG)lists[active]);
        }
        custom->dmaconr = (frame & 1) ? DMAF_BLITTER : 0;
        custom->vposr = (frame % 8) << 8;
        interruptHandler();
        assert(!pending_display);
        WaitDisplayList();
        assert(custom->cop2lc == (ULONG)lists[draw]);
        assert(custom->copjmp1 == 0);
        assert(custom->dmacon == 0);
        active = draw;
    }
    /* Render the third set while the second waits for presentation. Force
     * acceptance at every possible word boundary, then recycle only after
     * acknowledgement. Mirrors the production 0/1/2 buffer rotation. */
    for (unsigned accept=0;accept<=128;++accept) {
        UWORD triple[3][128]={{0}};
        unsigned display=0, pending=1, draw=2;
        pending_display=0;
        custom->cop2lc=(ULONG)triple[display];
        for(unsigned frame=1;frame<=30;++frame) {
            for(unsigned i=0;i<128;++i)triple[pending][i]=frame;
            QueueDisplayList(triple[pending]);
            for(unsigned i=0;i<=128;++i) {
                if(i==accept) {custom->vposr=0;interruptHandler();}
                if(i<128)triple[draw][i]=frame+1;
                assert(custom->cop2lc!=(ULONG)triple[draw]);
                for(unsigned j=0;j<128;++j)assert(triple[pending][j]==frame);
            }
            WaitDisplayList();
            assert(custom->cop2lc==(ULONG)triple[pending]);
            for(unsigned j=0;j<128;++j)triple[display][j]=0;
            unsigned retired=display;display=pending;pending=draw;draw=retired;
        }
    }
    puts("Display handoff: partial writes, delayed IRQs, repeated frames and buffer reuse passed");
}
'''
with tempfile.TemporaryDirectory() as directory:
    path = Path(directory)
    (path / 'test.c').write_text(header + queue + irq + test)
    subprocess.run([os.environ.get('HOST_CC', 'cc'), '-std=gnu99', '-O2',
                    '-Wall', '-Wextra', '-Werror', str(path / 'test.c'),
                    '-o', str(path / 'test')], check=True)
    subprocess.run([str(path / 'test')], check=True)
