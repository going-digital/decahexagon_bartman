#!/usr/bin/env python3
"""Verify artifacts from the disposable native save/reboot emulator trial."""
import hashlib,json,struct,zlib
from pathlib import Path
root=Path(__file__).resolve().parents[2];p=root/'scratchpad/trackloader/native_game'
original=(p/'native_menu.adf').read_bytes();saved=(p/'save_write.adf').read_bytes()
build=json.loads((root/'docs/TRACKLOADER_NATIVE_BOOT_RESULTS.json').read_text())
assert hashlib.sha256(original).hexdigest()==build['adf_sha256']
changed=[i for i in range(1760) if original[i*512:(i+1)*512]!=saved[i*512:(i+1)*512]]
assert changed==[1738,1749],changed
rows=[]
for sector,generation in [(1738,1),(1749,2)]:
 b=saved[sector*512:(sector+1)*512]
 assert struct.unpack_from('>4I',b)==(0x48585331,1,512,generation)
 assert b[16:32]==(p/'save_identity.bin').read_bytes()
 assert struct.unpack_from('>I',b,508)[0]==zlib.crc32(b[:508])
 assert not any(b[64:508])
 rows.append(dict(sector=sector,generation=generation,best_ticks=list(struct.unpack_from('>6I',b,32))))
assert rows[0]['best_ticks'][0]>0 and rows[1]['best_ticks'][0]==rows[0]['best_ticks'][0]
assert rows[1]['best_ticks'][1]>0
for name,events in {
 'save_write':['NATIVE-SAVE-DEFAULT','NATIVE-GAME-ENTRY','DF0:courtesyNATIVE-TUNE-READY'],
 'save_second':['NATIVE-SAVE-RESTORED','NATIVE-GAME-ENTRY','DF0:otisNATIVE-TUNE-READY'],
 'save_reboot':['NATIVE-SAVE-RESTORED','NATIVE-GAME-ENTRY'],
 'save_protected':['NATIVE-SAVE-DEFAULT','NATIVE-GAME-ENTRY','DF0:courtesyNATIVE-TUNE-READY']}.items():
 assert (p/(name+'.log')).read_text().splitlines()==events
 assert (p/(name+'.png')).exists()
report=dict(status='Native DiskIO save, alternate-slot update and cold-boot restoration passed',emulator='Copperline PAL A500OCS, 512K Chip + 512K slow',source_sha256=build['adf_sha256'],saved_sha256=hashlib.sha256(saved).hexdigest(),slots=rows,changed_sectors=changed,screenshots=[str(p/(n+'.png')) for n in ['save_write','save_second','save_reboot','save_protected']],memory=dict(stage_bytes=build['stage_bytes'],stage_spare=22528-build['stage_bytes'],game_memory=json.loads((root/'docs/TRACKLOADER_EXECUTABLE_PACKAGE_RESULTS.json').read_text())['memory_bytes'],game_spare=build['image_capacity']-json.loads((root/'docs/TRACKLOADER_EXECUTABLE_PACKAGE_RESULTS.json').read_text())['memory_bytes']),limitations=['Artifact verifier assumes captures belong to this image; screenshots reviewed separately','No physical hardware or NTSC test','No power-loss or mid-write disk-removal emulator test','Achievement triggers remain undefined; stored opaque bits preserved','Build-specific identity; cross-build migration absent'])
(root/'docs/TRACKLOADER_NATIVE_SAVE_RESULTS.json').write_text(json.dumps(report,indent=2)+'\n')
print(json.dumps(report,indent=2))
