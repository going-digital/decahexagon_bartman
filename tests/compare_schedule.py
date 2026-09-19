#!/usr/bin/env python3
"""Three normal base-level selectors versus saved original-machine-code traces."""
import csv, gzip, subprocess
from pathlib import Path
root=Path(__file__).resolve().parent.parent
rows=[]
for name in ('pc_selection_normal.csv.gz','pc_selection_hexagoner.csv.gz','pc_selection_hexagonest.csv.gz'):
    with gzip.open(root/'tests/fixtures'/name,'rt') as f:
        rows.extend(csv.DictReader(f))
inputs=''.join(' '.join(r[k] for k in ('stage','wave','sides','score','shape_counter','seed'))+'\n' for r in rows)
actual=subprocess.run([str(root/'out/schedule_probe')],input=inputs,text=True,capture_output=True,check=True).stdout.splitlines()
assert len(actual)==len(rows)==184320
for r,line in zip(rows,actual):
    a=line.split(',')
    expected=[r[k] for k in ('chosen','next_wave','next_shape_counter','speed','delay','marker_wait')]
    assert a[0]==expected[0] and all(abs(float(x)-float(y))<1e-5 for x,y in zip(a[1:],expected[1:])),(r,a)
print('184320 Hexagon/Hexagoner/Hexagonest native selector cases matched')
