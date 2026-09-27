#!/usr/bin/env python3
from pathlib import Path
import gzip
root=Path(__file__).resolve().parents[1]
rows=[]
for line in gzip.decompress((root/'tests/fixtures/pc_morph_freeze_native.txt.gz').read_bytes()).decode().splitlines():
    v=list(map(int,line.split()))
    rows.append(' {'+','.join(str(n)+('u' if i==6 else '') for i,n in enumerate(v))+'},')
(root/'tests/morph_freeze_cases.inc').write_text('/* Generated from pc_morph_freeze_native.txt.gz. */\n'+'\n'.join(rows)+'\n')
