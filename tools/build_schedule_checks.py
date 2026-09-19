#!/usr/bin/env python3
"""Select native outcomes covering each (chosen ID, side count, speed)."""
import csv,gzip,math
from pathlib import Path
root=Path(__file__).resolve().parent.parent
seen=set();out=[]
with gzip.open(root/'tests/fixtures/pc_selection_normal.csv.gz','rt') as f:
 for r in csv.DictReader(f):
  key=(r['chosen'],r['sides'],r['speed'])
  if key in seen:continue
  seen.add(key)
  values=[int(float(r[k])) for k in ('wave','sides','score','shape_counter','seed')]
  values += [int(r['chosen']) if r['chosen'] else -1]
  values += [int(float(r[k])) for k in ('next_wave','next_shape_counter','speed')]
  values += [math.ceil(float(r['delay'])),int(r['marker_wait'])]
  out.append('    {'+','.join(map(str,values))+'},')
(root/'tests/schedule_cases.inc').write_text('/* Generated from original machine-code traces by tools/build_schedule_checks.py. */\n'+'\n'.join(out)+'\n')
print(len(out),'target selector cases')
