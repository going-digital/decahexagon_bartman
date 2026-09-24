#!/usr/bin/env python3
"""Reproduce bounded-reader switching/media-swap checks in Copperline.

Uses disposable, write-protected images. Does not test empty-drive removal,
real hardware, audio quality or underruns. Screenshots need visual inspection.
"""
import argparse, hashlib, json, subprocess
from pathlib import Path
root=Path(__file__).resolve().parents[2];native=root/'scratchpad/trackloader/native_game'
p=argparse.ArgumentParser();p.add_argument('--rom',type=Path);p.add_argument('--verify-existing',action='store_true');p.add_argument('--video',choices=['PAL','NTSC'],default='PAL');p.add_argument('--scenario',choices=['all','ofs_switching','ofs_reinsert','ofs_midread'],default='all');args=p.parse_args()
if not args.verify_existing and not args.rom:p.error('--rom is required to run the emulator')
scenarios={
 'ofs_switching':dict(seconds=340,keys=[(60,0x40),(125,0x45),(126,0x4e),(127,0x40),(190,0x45),(191,0x4e),(192,0x40),(255,0x40),(270,0x45),(271,0x4f),(272,0x4f),(273,0x40)],swaps=[],events=['DF0:courtesyNATIVE-TUNE-READY','DF0:otisNATIVE-TUNE-READY','DF0:focusNATIVE-TUNE-READY','DF0:courtesyNATIVE-TUNE-READY']),
 'ofs_reinsert':dict(seconds=220,keys=[(60,0x40),(125,0x45),(126,0x4e),(127,0x40),(155,0x40)],swaps=[(120,'not_dos.adf'),(150,'native_menu.adf')],events=['DF0:courtesyNATIVE-TUNE-READY','NATIVE-TUNE-FAIL','DF0:otisNATIVE-TUNE-READY']),
 'ofs_midread':dict(seconds=170,keys=[(60,0x40),(100,0x40)],swaps=[(65,'not_dos.adf'),(95,'native_menu.adf')],events=['NATIVE-TUNE-FAIL','DF0:courtesyNATIVE-TUNE-READY'])
}
if args.scenario!='all':scenarios={args.scenario:scenarios[args.scenario]}
adf=native/'native_menu.adf';before=hashlib.sha256(adf.read_bytes()).hexdigest()
assert before==json.loads((root/'docs/TRACKLOADER_NATIVE_BOOT_RESULTS.json').read_text())['adf_sha256']
if not args.verify_existing:(native/'not_dos.adf').write_bytes(bytes(901120))
for name,case in scenarios.items():
 artifact=name+('_ntsc' if args.video=='NTSC' else '')
 log=native/(artifact+'.log');shot=native/(artifact+'.png')
 if not args.verify_existing:
  command=['copperline','--config',str(native/'native_menu.toml'),'--model','A500OCS','--chip','512K','--slow','512K','--video',args.video,'--noaudio','--serial','stdout']
  for when,key in case['keys']:command+=['--key-after',str(when),hex(key),'100']
  for when,disk in case['swaps']:command+=['--insert-disk-after',str(when),'0',str(native/disk)]
  command+=['--screenshot-after',str(case['seconds']),str(shot),str(args.rom)]
  with log.open('w') as stream:subprocess.run(command,cwd=root,stdout=stream,stderr=subprocess.STDOUT,check=True,timeout=600)
 lines=log.read_text().splitlines()
 assert lines==['NATIVE-SAVE-DEFAULT','NATIVE-GAME-ENTRY']+case['events'],(name,lines)
 assert shot.exists(),shot
 case['log']=str(log.relative_to(root));case['screenshot']=str(shot.relative_to(root))
assert hashlib.sha256(adf.read_bytes()).hexdigest()==before
report=dict(status='Expected native switching and media-swap serial sequences verified',adf_sha256=before,emulator='Copperline '+args.video+' A500OCS, 512K Chip + 512K slow',scenarios=scenarios,limitations=['Screenshots require separate visual review','No empty-drive or physical hardware check','No audio quality or underrun measurement','Verify-existing assumes logs belong to the recorded image'])
(root/('docs/TRACKLOADER_OFS_SWITCHING_RESULTS'+('_NTSC' if args.video=='NTSC' else '')+('_'+args.scenario.upper() if args.scenario!='all' else '')+'.json')).write_text(json.dumps(report,indent=2)+'\n')
print(args.video+': '+str(len(scenarios))+' switching/media-swap sequences match; disk image unchanged')
