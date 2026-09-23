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
               source.index('static struct View *ActiView;')]
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
    ULONG vposr, cop1lc;
} registers, *custom = &registers;
static unsigned frameCounter;
'''
test = r'''
int main(void) {
    UWORD lists[2][128] = {{0}};
    unsigned active = 0;
    custom->cop1lc = (ULONG)lists[active];
    for (unsigned frame = 1; frame <= 1000; ++frame) {
        unsigned draw = active ^ 1;
        /* Force VBlanks throughout construction, including between the
         * two words that would encode a bitplane pointer. */
        for (unsigned word = 0; word < 128; ++word) {
            lists[draw][word] = frame;
            custom->vposr = 0;
            interruptHandler();
            assert(custom->cop1lc == (ULONG)lists[active]);
            for (unsigned i = 0; i < 128; ++i)
                assert(lists[active][i] == frame - 1);
        }
        QueueDisplayList(lists[draw]);
        /* Late IRQs on either video standard must leave ownership pending. */
        const unsigned late[] = {16, 44, 100, 255, 261, 311};
        for (unsigned i = 0; i < sizeof(late)/sizeof(late[0]); ++i) {
            custom->vposr = late[i] << 8;
            interruptHandler();
            assert(pending_display == lists[draw]);
            assert(custom->cop1lc == (ULONG)lists[active]);
        }
        custom->dmaconr = (frame & 1) ? DMAF_BLITTER : 0;
        custom->vposr = (frame % 16) << 8;
        interruptHandler();
        assert(!pending_display);
        WaitDisplayList();
        assert(custom->cop1lc == (ULONG)lists[draw]);
        assert(custom->copjmp1 == 0x7fff);
        assert(custom->dmacon == (DMAF_SETCLR | custom->dmaconr));
        active = draw;
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
