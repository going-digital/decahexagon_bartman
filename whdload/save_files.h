#pragma once
#include "../trackloader/save_disk.h"
/* slot is 0/1, never a physical sector; missing files read as blank records. */
typedef int (*WhdSaveIO)(unsigned write,unsigned slot,void *bytes);
int whd_save_commit(const TrackSave *snapshot,const unsigned char *identity,
                    TrackSaveDiskScratch *scratch,WhdSaveIO io);
