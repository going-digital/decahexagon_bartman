#!/usr/bin/env python3
from pathlib import Path
import gzip
root=Path(__file__).resolve().parents[1]
rows=[]
for line in gzip.decompress((root/'tests/fixtures/pc_death_native.txt.gz').read_bytes()).decode().splitlines():
    v=list(map(int,line.split()))
    if (v[0]==0 and v[1] in (0,1,2,6,8,10) and v[3] in (1,59,72,85,120)) or (v[:4]==[1,0,0,1]):
        rows.append(' {'+','.join(str(n)+('u' if i==10 else '') for i,n in enumerate(v))+'},')
(root/'tests/death_cases.inc').write_text('/* Generated from pc_death_native.txt.gz. */\n'+'\n'.join(rows)+'\n')
