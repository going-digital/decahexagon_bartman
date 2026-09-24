#!/usr/bin/env python3
"""Verify disassembled disk drivers against hashes captured before replacement."""
import hashlib,json,subprocess,tempfile
from pathlib import Path
root=Path(__file__).resolve().parents[1]
expected=json.loads((root/'tests/diskio_original_hashes.json').read_text())
with tempfile.TemporaryDirectory(prefix='diskio-disassembly-') as tmp:
 tmp=Path(tmp)
 for name,reference in expected.items():
  for offset in (0,32):
   source=tmp/'wrapper.s';target=tmp/'driver.bin'
   source.write_text(f' dcb.b {offset},0\n include "{name}.s"\n')
   subprocess.run(['vasmm68k_mot','-m68000','-Fbin','-quiet','-I'+str(root),'-o',str(target),str(source)],check=True)
   data=target.read_bytes()[offset:]
   assert len(data)==reference['bytes'],(name,'length',len(data))
   assert hashlib.sha256(data).hexdigest()==reference['sha256'],(name,'bytes changed')
  print(f'{name}: {reference["bytes"]} bytes match original; origin 0 and 32')
