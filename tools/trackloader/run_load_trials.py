#!/usr/bin/env python3
"""Cold-boot each immutable tune fixture through real disk, inflate and tune API."""
import hashlib,json,shutil,subprocess,sys
from pathlib import Path
root=Path(__file__).resolve().parents[2];out=root/'scratchpad/trackloader'
rom=Path.home()/'Documents/FS-UAE/Kickstarts/Kickstart v1.3 r34.005 (1987-12)(Commodore)(A500-A1000-A2000-CDTV)[!].rom'
rows=[]
for track in ['courtesy','focus','otis']:
 subprocess.run([sys.executable,str(root/'tools/trackloader/build_load_boot.py'),'--track',track],cwd=root,check=True)
 disk=out/f'load_{track}.adf';shutil.copyfile(out/'load_trial.adf',disk)
 before=hashlib.sha256(disk.read_bytes()).hexdigest()
 config=out/f'load_{track}.toml';config.write_text(f'[emulation]\npacing_budget = "cycles"\n[floppy.df0]\npath = "{disk}"\nwrite_protected = true\n')
 log=out/f'load_{track}.log'
 with log.open('w') as f:
  result=subprocess.run(['copperline','--config',str(config),'--model','A500OCS','--chip','512K','--slow','512K','--video','PAL','--noaudio','--serial','stdout','--screenshot-after','90',str(out/f'load_{track}.png'),str(rom)],stdout=f,stderr=subprocess.STDOUT,timeout=150)
 assert result.returncode==0 and 'TRACKLOAD-PASS' in log.read_text(),log
 assert hashlib.sha256(disk.read_bytes()).hexdigest()==before
 row=json.loads((out/f'{track}.load_manifest.json').read_text());row.update(passed=True,image_unchanged=True,stage_bytes=(out/'load_stage.bin').stat().st_size)
 rows.append(row);print(track,'PASS',flush=True)
(root/'docs/TRACKLOADER_ALL_TUNES_BOOT_RESULTS.json').write_text(json.dumps({'status':'Three independent PAL A500 cold boots; real disk and validated C tune preparation, no audio/game yet','trials':rows},indent=2)+'\n')
