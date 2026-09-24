#include "../tune.h"
_Static_assert(sizeof(TrackTuneInfo)<=64,"resident cache info region");
_Static_assert(sizeof(PcmSong)<=32,"resident cache binding region");
unsigned resident_tune_prepare(unsigned char *arena,unsigned bytes,unsigned capacity,
                              PcmSong *song,TrackTuneInfo *info) {
    if(!trackloader_tune_prepare(arena,bytes,capacity,song,info))return 0;
    return 1;
}
