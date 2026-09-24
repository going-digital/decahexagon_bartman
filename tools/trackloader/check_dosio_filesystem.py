#!/usr/bin/env python3
"""Build an OFS image using the original DosIO code on an emulated 68000."""
import argparse,hashlib,json,struct,subprocess,sys
from pathlib import Path
p=argparse.ArgumentParser();p.add_argument('--execram-source',type=Path,required=True);a=p.parse_args()
root=Path(__file__).resolve().parents[2];out=root/'scratchpad/trackloader';v=a.execram_source/'src/musashi_vendor'
# Share the same writer build as the production image pipeline.
subprocess.run([sys.executable,str(root/'tools/trackloader/build_dosio_probe.py'),'--execram-source',str(a.execram_source)],check=True)
args=[str(out/'dosio_probe'),str(out/'dosio.bin'),str(out/'filesystem_trial.adf')]
for track in ['courtesy','focus','otis']:args += ['DF0:'+track,str(out/f'{track}.fibonacci.deflate')]
args+=['DF0:game',str(out/'game_budget/out/game_packed.exe')]
for name,byte in [('save_a',17),('save_b',34),('save_a',51)]:
 path=out/f'{name}_{byte}.test';path.write_bytes(bytes([byte])*512);args+=['DF0:'+name,str(path)]
r=subprocess.run(args,capture_output=True,text=True,check=True);print(r.stdout)
disk=(out/'filesystem_trial.adf').read_bytes();assert len(disk)==901120
rootblock=struct.unpack('>128I',disk[880*512:881*512]);assert sum(rootblock)&0xffffffff==0
bitmap_sector=rootblock[79];bitmap=struct.unpack('>128I',disk[bitmap_sector*512:(bitmap_sector+1)*512]);assert sum(bitmap)&0xffffffff==0
free=sum(bool(bitmap[1+(s-2)//32]&(1<<((s-2)%32))) for s in range(2,1760))
report={'status':'Real DosIO 68000 filesystem code; sector I/O mocked, not DMA validation; image is not bootable','image_sha256':hashlib.sha256(disk).hexdigest(),'free_sectors':free,'free_bytes':free*512,'save_slots':'Two 512-byte test files, overwrite/readback tested; crash-safe placement not yet implemented','log':r.stdout.splitlines()}
(root/'docs/TRACKLOADER_FILESYSTEM_RESULTS.json').write_text(json.dumps(report,indent=2)+'\n');print(json.dumps(report,indent=2))
