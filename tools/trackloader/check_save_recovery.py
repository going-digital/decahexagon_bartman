#!/usr/bin/env python3
"""Cold-boot save selection with actual game RAM checks on disposable disks."""
import argparse, hashlib, json, struct, subprocess, time, zlib
from pathlib import Path
from package_native_game import elf_image
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--rom',type=Path,required=True)
p.add_argument('--video',choices=['PAL','NTSC'],required=True)
a=p.parse_args()
root=Path(__file__).resolve().parents[2];base=root/'scratchpad/trackloader/native_game'
out=base/f'recovery_{a.video.lower()}_{time.time_ns()}';out.mkdir()
original=(base/'native_menu.adf').read_bytes();digest=hashlib.sha256(original).hexdigest()
assert digest==json.loads((root/'docs/TRACKLOADER_NATIVE_BOOT_RESULTS.json').read_text())['adf_sha256']
identity=(base/'save_identity.bin').read_bytes()
image,_,_,relocs,_=elf_image(base/'game.elf')
sdk=next((Path.home()/'.vscode/extensions').glob('bartmanabyss.amiga-debug-*/bin/darwin/opt/m68k-amiga-elf/bin/nm'))
listing=subprocess.check_output([str(sdk),str(base/'game.elf')],text=True)
symbols={s.split()[-1]:int(s.split()[0],16) for s in listing.splitlines() if len(s.split())==3}
sig_offset=next(i for i in range(0,len(image)-64,64) if not any(i-3<=r<i+64 for r in relocs))
signature=bytes(image[sig_offset:sig_offset+64])
def slot(generation,ticks):
 b=bytearray(512);struct.pack_into('>4I',b,0,0x48585331,1,512,generation);b[16:32]=identity
 struct.pack_into('>I',b,32,ticks);struct.pack_into('>II',b,56,1,0x12345678)
 struct.pack_into('>I',b,508,zlib.crc32(b[:508]));return b
rows=[]
for case in ('latest','fallback','foreign','generation_wrap'):
 work=out/case;work.mkdir();first=slot(1,3901);second=slot(2,4501)
 expected=(4501,2,1,0x12345678)
 if case=='fallback':second[100]^=1;expected=(3901,1,1,0x12345678)
 if case=='foreign':
  for b in (first,second):b[16]^=1;struct.pack_into('>I',b,508,zlib.crc32(b[:508]))
  expected=(0,0,0,0)
 if case=='generation_wrap':first=slot(0xffffffff,3901);second=slot(0,4501);expected=(4501,0,1,0x12345678)
 disk=bytearray(original);disk[1738*512:1739*512]=first;disk[1749*512:1750*512]=second
 path=work/'disk.adf';path.write_bytes(disk)
 config=work/'config.toml';config.write_text('[emulation]\npacing_budget = "cycles"\n[floppy.df0]\npath = '+json.dumps(str(path))+'\nwrite_protected = true\n')
 info=work/'control.json'
 def call(method,params=None):
  result=json.loads(subprocess.check_output(['copperline-ctl','--info',str(info),method,json.dumps(params or {})],text=True,timeout=120))
  if 'error' in result:raise RuntimeError(result)
  return result['result']
 def read(addr,n):return bytes.fromhex(call('mem.read',dict(addr=addr,len=n,encoding='hex'))['data'])
 with (work/'serial.log').open('w') as log:
  proc=subprocess.Popen(['copperline','--config',str(config),'--model','A500OCS','--chip','512K','--slow','512K','--video',a.video,'--noaudio','--serial','stdout','--control',':0','--control-info',str(info),str(a.rom)],stdout=log,stderr=log)
  try:
   for _ in range(100):
    if info.exists():break
    if proc.poll() is not None:raise RuntimeError('Emulator exited')
    time.sleep(.1)
   call('run_until',dict(seconds=55))
   memory=b''.join(read(addr,32768) for addr in range(0,524288,32768))
   location=memory.find(signature);assert location>=0 and memory.find(signature,location+1)<0
   gamebase=location-sig_offset
   records=read(gamebase+symbols['records'],30)
   best=struct.unpack_from('>6I',records);completed=list(records[24:30])
   generation=struct.unpack('>I',read(gamebase+symbols['save_generation'],4))[0]
   achievements=struct.unpack('>I',read(gamebase+symbols['save_achievements'],4))[0]
   assert (best[0],generation,completed[0],achievements)==expected,(case,best,completed,generation,achievements)
   assert not any(best[1:]) and not any(completed[1:])
   call('capture.screenshot',dict(path=str(work/'menu.png')))
   call('shutdown');proc.wait(timeout=10)
  finally:
   if proc.poll() is None:proc.terminate();proc.wait(timeout=10)
 expected_log=['NATIVE-SAVE-DEFAULT' if case=='foreign' else 'NATIVE-SAVE-RESTORED','NATIVE-GAME-ENTRY']
 assert [line for line in (work/'serial.log').read_text().splitlines() if not line.startswith('copperline-control: ')]==expected_log
 assert path.read_bytes()==disk
 rows.append(dict(case=case,best_ticks=list(best),completed=completed,generation=generation,achievements=achievements,disk_unchanged=True))
 print(a.video,case,'passed',flush=True)
assert (base/'native_menu.adf').read_bytes()==original
report=dict(status=a.video+' native cold-boot recovery verified in game RAM',source_sha256=digest,trials=rows,evidence=str(out.relative_to(root)),limitations=['Seeded save fixtures, not physical interrupted writes','Achievement bits tested as opaque storage; no achievement triggers tested','No physical hardware validation'])
(root/f'docs/TRACKLOADER_SAVE_RECOVERY_{a.video}_RESULTS.json').write_text(json.dumps(report,indent=2)+'\n')
