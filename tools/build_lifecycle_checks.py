#!/usr/bin/env python3
"""Select compact lifecycle boundary expectations from the executed-PC fixture."""
from pathlib import Path
import gzip
root=Path(__file__).resolve().parents[1]
rows=[]
for line in gzip.decompress((root/'tests/fixtures/pc_lifecycle_native.txt.gz').read_bytes()).decode().splitlines():
    v=line.split()
    if v[0]=='D' and v[1]=='0' and int(v[3]) in (1,58,59,71,72,73,74,85):
        rows.append(' {'+','.join(v[2:7])+'},')
(root/'tests/lifecycle_cases.inc').write_text('/* Generated from pc_lifecycle_native.txt.gz; do not edit. */\n'+'\n'.join(rows)+'\n')
