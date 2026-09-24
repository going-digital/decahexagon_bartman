#!/usr/bin/env python3
"""Exercise actual game storage-boundary functions with portable records logic."""
from pathlib import Path
import subprocess,tempfile
root=Path(__file__).resolve().parents[1];source=(root/'game.c').read_text()
def function(name):
 start=source.index(name+'(');start=source.rfind('\n',0,start)+1
 end=source.index('{',start)+1;depth=1
 while depth:
  depth+=(source[end]=='{')-(source[end]=='}');end+=1
 return source[start:end]
code='''
#include <assert.h>
#include "trackloader/save.h"
typedef unsigned char UBYTE;
typedef unsigned short UWORD;
enum {MODE_ATTRACT,MODE_PLAYING,MODE_DEAD,MODE_GAMEOVER};
static int mode;
static PcRecords records;
static uint32_t save_generation,save_achievements;
static UBYTE save_dirty,save_restore_open,selected_profile,new_record;
static struct {UWORD time_seconds,time_subsecond_frames,record_seconds,record_subsecond_frames;} gamestate;
'''
code+='\n'.join(function(n) for n in ['load_record','game_save_dirty','game_restore_save','game_save_snapshot','game_save_committed','record_time'])
code+='''
int main(void) {
 TrackSave loaded={0},snapshot={0},newer={0};
 save_restore_open=1;loaded.generation=0xffffffffU;loaded.achievements=0x55aa;
 loaded.records.best[0]=3601;loaded.records.completed[0]=1;
 assert(game_restore_save(&loaded));assert(!game_restore_save(&loaded));
 assert(gamestate.record_seconds==60 && gamestate.record_subsecond_frames==1);
 assert(pc_profile_unlocked(&records,3) && !pc_profile_unlocked(&records,4));
 assert(!game_save_dirty() && !game_save_snapshot(&snapshot));
 gamestate.time_seconds=60;gamestate.time_subsecond_frames=2;
 record_time();assert(game_save_dirty() && new_record);
 mode=MODE_PLAYING;assert(!game_save_snapshot(&snapshot));
 mode=MODE_DEAD;assert(!game_save_snapshot(&snapshot));
 mode=MODE_GAMEOVER;assert(game_save_snapshot(&snapshot));
 assert(snapshot.generation==0 && snapshot.achievements==loaded.achievements);
 /* A failed or write-protected save produces no acknowledgement. */
 assert(game_save_dirty());assert(game_save_snapshot(&newer));
 assert(newer.generation==snapshot.generation);
 gamestate.time_subsecond_frames=3;record_time();
 assert(game_save_committed(&snapshot) && game_save_dirty());
 assert(!game_save_committed(&snapshot)); /* duplicate/stale acknowledgement */
 mode=MODE_ATTRACT;assert(game_save_snapshot(&newer));assert(newer.generation==1);
 assert(newer.records.best[0]==3603 && game_save_committed(&newer));
 assert(!game_save_dirty() && !game_save_snapshot(&snapshot));
 record_time();assert(!game_save_dirty());
 /* Completion-only changes also become dirty, even without a new best. */
 records.completed[0]=0;records.best[0]=5000;new_record=0;record_time();
 assert(game_save_dirty() && !new_record && records.completed[0]);
 assert(!game_restore_save(&loaded));
 save_dirty=0;save_restore_open=1;loaded.records.completed[0]=2;
 uint32_t before=records.best[0];assert(!game_restore_save(&loaded));
 assert(records.best[0]==before && save_restore_open);
 assert(!game_restore_save(0) && !game_save_snapshot(0) && !game_save_committed(0));
 return 0;
}
'''
with tempfile.TemporaryDirectory() as temp:
 path=Path(temp);(path/'test.c').write_text(code)
 subprocess.run(['cc','-std=c99','-Wall','-Wextra','-Werror','-I'+str(root),str(path/'test.c'),str(root/'pc_menu.c'),'-o',str(path/'test')],check=True)
 subprocess.run([str(path/'test')],check=True)
print('Game save boundary: restore, unlocks, dirty coalescing, safe snapshots, failed writes, stale acknowledgements and generation wrap pass')
