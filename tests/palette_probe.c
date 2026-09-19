#include "../pc_palette.h"
#include <stdio.h>
int main(void) {
    PcPalette p;
    for (int stage=0;stage<3;++stage) for (int hyper=0;hyper<2;++hyper) {
        pc_palette_start(&p,(uint8_t)stage,(uint8_t)hyper);
        printf("P %d %d %u",stage,hyper,p.scheme);
        for (int i=0;i<6;++i) printf(" %d",p.rgb[i]);
        printf("\n");
    }
    for (int stage=0;stage<3;++stage) {
        pc_palette_reset(&p,(uint8_t)stage);
        for (int tick=1;tick<=7300;++tick) {
            pc_palette_tick(&p,(uint32_t)tick);
            printf("T %d %d %d %d %d %d",stage,tick,p.scheme,p.change,p.blend,p.direction);
            for (int i=0;i<6;++i) printf(" %d",p.rgb[i]);
            printf(" %u %u\n",pc_palette_colour(&p,0),pc_palette_colour(&p,1));
        }
    }
}
