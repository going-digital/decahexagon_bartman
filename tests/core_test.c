#include <stdio.h>
#include "core_checks.h"
int main(void) {
    unsigned failed = pc_core_checks();
    if (failed) { fprintf(stderr,"PC core check failed at line %u\n",failed); return 1; }
    puts("PC core: timing, collision, waves, selector, morph and projection checks passed");
    return 0;
}
