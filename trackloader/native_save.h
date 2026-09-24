#pragma once
#include "game_boot.h"
void native_save_init(const TrackGameBoot *boot);
/* Main-thread only. Returns nonzero if a blocking save was attempted. */
int native_save_tick(void *display_plane);

#if WHDLOAD
int native_save_finish(void *display_plane);
#endif
