#!/usr/bin/env python3
"""Compare RGB/state against executed PC machine code, then OCS rounding."""
import gzip
from pathlib import Path
import subprocess
root=Path(__file__).resolve().parents[1]
with gzip.open(root/'tests/fixtures/pc_palette_native.txt.gz','rt') as f:
    lines=f.read().splitlines()
    expected=[l for l in lines if l.startswith('T ')]
    schemes={int(l.split()[1]):list(map(int,l.split()[2:8])) for l in lines if l.startswith('SCHEME ')}
actual=subprocess.check_output([str(root/'out/palette_probe')],text=True).splitlines()
profiles=[l for l in actual if l.startswith('P ')]
assert len(profiles)==6
for line in profiles:
    stage,hyper,scheme,*rgb=map(int,line.split()[1:])
    assert scheme==((3,6,9) if hyper else (0,5,10))[stage]
    assert rgb==schemes[scheme],line
actual=[l for l in actual if l.startswith('T ')]
assert len(expected)==len(actual)
for e,a in zip(expected,actual):
    fields=a.split()
    assert ' '.join(fields[:-2])==e, (e,a)
    rgb=list(map(int,e.split()[-6:]))
    for slot in range(2):
        r,g,b=rgb[slot*3:slot*3+3]
        packed=round(r/17)*256+round(g/17)*16+round(b/17)
        assert int(fields[-2+slot])==packed, (e,a,packed)
print(f'PC palette reference: {len(expected):,} ticks match RGB, transitions and OCS output; six profile starts match')
