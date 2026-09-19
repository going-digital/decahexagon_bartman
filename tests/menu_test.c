#include <stdio.h>
unsigned pc_menu_checks(void);
int main(void) {
    unsigned fail=pc_menu_checks();
    if (fail) { printf("Menu failure %u\n",fail);return 1; }
    puts("Menu/session checks pass: wrap, input priority, threshold, isolated records and unlocks");return 0;
}
