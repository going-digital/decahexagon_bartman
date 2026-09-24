#!/usr/bin/env python3
"""Prepare a private FS-UAE hard-drive test using user-supplied AmigaOS media."""
import argparse,json,shutil,subprocess,time
from pathlib import Path
p=argparse.ArgumentParser();p.add_argument('--workbench',type=Path,required=True);p.add_argument('--modules',type=Path,required=True);p.add_argument('--rom',type=Path,required=True);p.add_argument('--xdftool',default='xdftool');p.add_argument('--slots',type=int,choices=(1,2,3),default=3);p.add_argument('--video',choices=('PAL','NTSC'),default='PAL');p.add_argument('--cpu',choices=('68000','68020','68030','68060'),default='68020');p.add_argument('--saves',type=Path);p.add_argument('--no-preload',action='store_true');a=p.parse_args()
root=Path(__file__).resolve().parents[2];run=root/'scratchpad/whdload'/f'test_{time.time_ns()}';run.mkdir()
os=run/'system';modules=run/'modules'
for source,dest in [(a.workbench,os),(a.modules,modules)]:subprocess.run([a.xdftool,str(source.resolve()),'unpack',str(dest)],check=True)
# Amiga paths are case-insensitive; keep one host spelling for each directory.
for name in ('LIBS','L','DEVS'):
 src=modules/name
 target=next((d for d in os.iterdir() if d.name.lower()==name.lower()),os/name)
 if src.exists():shutil.copytree(src,target,dirs_exist_ok=True)
if (os/'WBStartup').exists():
 (os/'WBStartup').rename(os/'WBStartup-disabled')
 (os/'WBStartup').mkdir()
shutil.copyfile(root/'scratchpad/whdload/sdk/WHDLoad/C/WHDLoad',os/'C/WHDLoad')
shutil.copytree(root/'out/whdload/Hexagon',os/'Hexagon')
if a.saves:
 for name in ('save-a','save-b'):
  if (a.saves/name).is_file():shutil.copyfile(a.saves/name,os/'Hexagon/data'/name)
(os/'S/startup-sequence').write_text('Assign ENV: RAM:\nAssign T: RAM:\nPath C: ADD\nLoadWB\nCD SYS:Hexagon\n'+f'WHDLoad SLAVE=Hexagon-{a.slots}.slave '+('' if a.no_preload else 'PRELOAD ')+'NOWRITECACHE SPLASHDELAY=0 >SYS:whd-result.txt\nEcho RETURNED >SYS:returned.txt\nEndCLI\n')
model='A500+' if a.cpu=='68000' else 'A1200'
(run/'check.fs-uae').write_text(f'''[fs-uae]
amiga_model = {model}
cpu = {a.cpu}
chip_memory = 2048
fast_memory = 4096
ntsc_mode = {int(a.video=='NTSC')}
kickstart_file = {a.rom.resolve()}
hard_drive_0 = {os}
hard_drive_0_label = System
base_dir = {run}
screenshots_output_dir = {run}
screenshots_output_mask = 3
window_width = 800
window_height = 600
fullscreen = 0
automatic_input_grab = 0
initial_input_grab = 0
''')
(run/'test.json').write_text(json.dumps(dict(slots=a.slots,video=a.video,cpu=a.cpu,preload=not a.no_preload,manifest=json.loads((os/'Hexagon/manifest.json').read_text())),indent=2)+'\n')
print(run/'check.fs-uae')
