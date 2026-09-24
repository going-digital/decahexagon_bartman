#!/usr/bin/env python3
"""Seed disposable native disks for read-only cold-boot recovery checks."""
import hashlib,json,struct,zlib
from pathlib import Path
root=Path(__file__).resolve().parents[2];native=root/'scratchpad/trackloader/native_game'
manifest=json.loads((native/'save_manifest.json').read_text());original=(native/'native_menu.adf').read_bytes()
assert hashlib.sha256(original).hexdigest()==manifest['image_sha256']
identity=bytes.fromhex(manifest['identity']);assert identity==(native/'save_identity.bin').read_bytes()
def slot(generation,ticks):
 b=bytearray(512);struct.pack_into('>4I',b,0,0x48585331,1,512,generation);b[16:32]=identity
 struct.pack_into('>I',b,32,ticks);struct.pack_into('>I',b,56,1)
 struct.pack_into('>I',b,508,zlib.crc32(b[:508]));return b
for name in ['save_latest','save_fallback','save_foreign']:
 disk=bytearray(original);a=slot(1,3901);b=slot(2,4501)
 if name=='save_fallback':b[100]^=1
 if name=='save_foreign':
  for block in [a,b]:
   block[16]^=1;struct.pack_into('>I',block,508,zlib.crc32(block[:508]))
 disk[1738*512:1739*512]=a;disk[1749*512:1750*512]=b
 path=native/(name+'.adf');path.write_bytes(disk)
 (native/(name+'.toml')).write_text(f'[emulation]\npacing_budget = "cycles"\n[floppy.df0]\npath = "{path}"\nwrite_protected = true\n')
 print(name,hashlib.sha256(disk).hexdigest())
