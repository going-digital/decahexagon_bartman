#!/usr/bin/env python3
"""Full-minute PC/port morph scheduling and active-wall comparison."""
from pathlib import Path
import gzip, subprocess, sys, hashlib
root=Path(__file__).resolve().parents[1]
subprocess.run(['cc','-O2','-I'+str(root),*[str(root/p) for p in ['tests/morph_run_probe.c','pc_core.c','pc_world.c','pc_morph.c','pc_schedule.c','pc_waves.c']],'-o',str(root/'out/morph_run_probe')],check=True)
fixture=root/'tests/fixtures/pc_morph_run_native.txt.gz'
if '--capture' in sys.argv:
 binary=Path.home()/'Library/Application Support/Steam/steamapps/common/Super Hexagon/Super Hexagon.app/Contents/MacOS/SuperHexagon'
 assert hashlib.sha256(binary.read_bytes()).hexdigest()=='91f10469fbbefffed3248c583306e1aef442b4dea55a400e79df09aff2cfaf88'
 subprocess.run(['clang','-arch','x86_64','-O0',str(root/'tools/probe_pc_morph_run.c'),'-o',str(root/'out/pc_morph_run_probe')],check=True)
 rows=[]
 for seed in range(1,9):
  output=subprocess.check_output([str(root/'out/pc_morph_run_probe'),str(binary),str(seed)])
  rows.extend(str(seed).encode()+b' '+line for line in output.splitlines())
 fixture.write_bytes(gzip.compress(b'\n'.join(rows)+b'\n',mtime=0))
rows=gzip.decompress(fixture.read_bytes()).splitlines()
assert len(rows)==28800
for seed in range(1,9):
 expected=[r.split(b' ',1)[1] for r in rows if r.startswith(str(seed).encode()+b' ')]
 actual=subprocess.check_output([str(root/'out/morph_run_probe'),str(seed)]).splitlines()
 assert len(actual)==len(expected)==3600
 for tick,(a,b) in enumerate(zip(actual,expected),1):assert a==b,(seed,tick,a,b)
print('28,800 native PC ticks match: active walls, waves, speed, morphs and RNG draws')
