#include "pc_ending_camera.h"
int16_t pc_ending_seek_angle(int16_t angle,int16_t target) {
    return angle<target ? angle+1 : angle>target ? angle-1 : angle;
}
int16_t pc_ending_null_angle(int16_t angle) {
    return pc_ending_seek_angle(angle,0);
}
int16_t pc_ending_otis_angle(int16_t angle,unsigned wave_mode) {
    /* Unlike seekangle, otisrotate does not correct an angle already beyond
     * the limit. Modes 0/1 use 10 degrees; modes 2..9 use 20 degrees. */
    if(wave_mode>9)return angle;
    int limit=wave_mode<2 ? 10:20;
    if(wave_mode&1)return angle<limit ? angle+1:angle;
    return angle>-limit ? angle-1:angle;
}
void pc_ending_camera_reference_tick(PcEndingCamera *camera,const PcEnding *ending) {
    if(ending->wait)return;
    switch(ending->phase) {
    case 0: case 1: case 2: case 3: case 4:
        camera->otis=pc_ending_null_angle(camera->otis);break;
    case 6: case 7: case 8:
        camera->otis=pc_ending_otis_angle(camera->otis,ending->wave_mode);
        camera->tilt=pc_ending_seek_angle(camera->tilt,20);break;
    case 9:
        camera->otis=pc_ending_null_angle(camera->otis);
        camera->tilt=pc_ending_seek_angle(camera->tilt,30);break;
    case 11: case 12:
        camera->tilt=pc_ending_seek_angle(camera->tilt,30);break;
    case 13:
        camera->tilt=pc_ending_seek_angle(camera->tilt,0);break;
    default: break;
    }
}

void pc_ending_camera_tick(PcEndingCamera *camera,const PcEnding *ending) {
    (void)ending;camera->tilt=0;camera->otis=0;
}
