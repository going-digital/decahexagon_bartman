#include "../pc_palette.h"
#include <assert.h>
#include <stdio.h>

int main(int argc,char **argv) {
    assert(argc==2);
    FILE *f=fopen(argv[1],"r");assert(f);
    unsigned endpoints[12],id;
    assert(fscanf(f,"SCHEME %u",&id)==1 && id==200);
    for(unsigned i=0;i<12;i++)assert(fscanf(f,"%u",&endpoints[i])==1);
    const unsigned targets[]={9,10,7,6,5,1,3,0,200,30};
    PcPalette p;pc_palette_ending_start(&p);
    unsigned tick,scheme,change,blend,direction,rgb[6],count=0;
    while(fscanf(f," T %u %u %u %u %u",&tick,&scheme,&change,&blend,&direction)==5) {
        assert(tick==++count);
        for(unsigned i=0;i<6;i++)assert(fscanf(f,"%u",&rgb[i])==1);
        pc_palette_tick(&p,0);
        if(tick%100==1)pc_palette_request(&p,targets[(tick-1)/100]);
        assert(p.scheme==scheme && p.change==change);
        assert((int)p.blend==(int)blend && p.direction==direction);
        for(unsigned i=0;i<6;i++)assert(p.rgb[i]==rgb[i]);
        if(p.scheme==200)for(unsigned i=0;i<6;i++) {
            assert(p.start[i]==endpoints[i]);
            assert(p.end[i]==endpoints[i+6]);
        }
    }
    assert(feof(f));fclose(f);assert(count==1000);
    puts("Ending palettes: all 1,000 native transition ticks match");
}
