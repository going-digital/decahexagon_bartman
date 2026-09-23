#include "coplist.h"

// COP1 automatically restarts here each frame. VBlank may replace COP2LC
// before line 8; the copper follows it at line 16. BFD=1 ignores the blitter.
const UWORD copper_dispatch[] __attribute__((section (".MEMF_CHIP"))) = {
    0x1005, 0xfffe,
    offsetof(struct Custom, copjmp2), 0x7fff
};
