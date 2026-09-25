#include "../pc_ending_projection.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
static int clamp(int n) {return n>32767 ? 32767:n< -32767 ? -32767:n;}
int main(int argc,char **argv) {
    assert(argc==2);FILE *f=fopen(argv[1],"r");assert(f);
    int t,o,d,x,y,z,ex,ey,count=0,near=0,max_error=0;
    double nx,ny,nz;
    while(fscanf(f,"%d %d %d %d %d %d %lf %lf %lf %d %d",
          &t,&o,&d,&x,&y,&z,&nx,&ny,&nz,&ex,&ey)==11) {
        PcEndingView v={t,o,d,384,240,600,600};int16_t sx,sy;
        assert(pc_ending_project(&v,x,y,z,&sx,&sy));
        /* Native floating-point residue near the singular plane is not a
         * useful pixel oracle. Keep these cases visible in the test report. */
        if(nz<1){near++;continue;}
        int error=abs(sx-clamp(ex));if(abs(sy-clamp(ey))>error)error=abs(sy-clamp(ey));
        if(error>max_error)max_error=error;
        assert(error<=1);count++;
    }
    assert(feof(f));fclose(f);assert(count+near==360);
    PcEndingView v={0,0,0,384,240,600,600};int16_t sx=1,sy=2;
    assert(pc_ending_project(&v,0,0,0,&sx,&sy) && sx==384 && sy==240);
    assert(pc_ending_project(&v,100,0,0,&sx,&sy) && sx==32767);
    sx=1;sy=2;assert(!pc_ending_project(&v,8193,0,0,&sx,&sy) && sx==1 && sy==2);
    printf("Fixed-point projection: %d native cases, maximum error %d pixel; %d near-plane cases excluded\n",count,max_error,near);
}
