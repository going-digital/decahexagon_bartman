#include "coplist.h"

// Put copperlist into chip mem so we can use it without copying
const UWORD copper2[] __attribute__((section (".MEMF_CHIP"))) = {
    0xffff, 0xfffe // End copper list
};
