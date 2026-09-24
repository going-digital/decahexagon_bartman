#include "../tune.h"
unsigned resident_tune_prepare(unsigned char *arena,unsigned bytes,unsigned capacity,
                              PcmSong *song,TrackTuneInfo *info) {
    if(!trackloader_tune_prepare(arena,bytes,capacity,song,info))return 0;
    return 1;
}
