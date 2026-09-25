#include "../pc_ending_camera.h"
#include <assert.h>
#include <stdio.h>
int main(int argc,char **argv) {
    assert(argc==2);FILE *f=fopen(argv[1],"r");assert(f);
    char kind;int mode,angle,expected,count=0;
    while(fscanf(f," %c %d %d %d",&kind,&mode,&angle,&expected)==4) {
        int actual;
        if(kind=='R')actual=pc_ending_otis_angle(angle,mode);
        else if(kind=='N')actual=pc_ending_null_angle(angle);
        else {assert(kind=='S');actual=pc_ending_seek_angle(angle,mode);}
        assert(actual==expected);count++;
    }
    assert(feof(f));fclose(f);assert(count==1936);
    /* Phase dispatch transcribed from gamelogic, separate from the native
     * single-routine fixture above. Incoming mode matters at transitions. */
    for(unsigned phase=0;phase<=255;phase++) {
        PcEnding ending={0};ending.phase=phase;ending.wave_mode=4;
        PcEndingCamera camera={15,5};
        pc_ending_camera_reference_tick(&camera,&ending);
        int tilt=15,otis=5;
        if(phase<=4 || (phase>=6 && phase<=9))otis=4;
        if((phase>=6 && phase<=9) || phase==11 || phase==12)tilt=16;
        if(phase==13)tilt=14;
        assert(camera.tilt==tilt && camera.otis==otis);
        ending.wait=1;camera=(PcEndingCamera){15,5};
        pc_ending_camera_reference_tick(&camera,&ending);
        assert(camera.tilt==15 && camera.otis==5);
        pc_ending_camera_tick(&camera,&ending);
        assert(camera.tilt==0 && camera.otis==0);
    }
    puts("Ending camera: all 1,936 native integral dt=1 cases match");
}
