#!/usr/bin/env python3
"""Compare new target C with saved original x86 machine-code output."""
import gzip,subprocess,json
from pathlib import Path
root=Path(__file__).resolve().parent.parent
out=subprocess.run([str(root/'out/wave_probe')],input=(root/'tests/fixtures/pc_wave_inputs.txt').read_text(),text=True,capture_output=True,check=True).stdout.splitlines()
reference=gzip.open(root/'tests/fixtures/pc_wave_native.txt.gz','rt').read().splitlines()
assert len(out)==len(reference)==9159
for a,b in zip(out,reference):
 a=a.split();b=b.split()
 assert a[:3]==b[:3] and a[4:]==b[4:],(a[:7],b[:7])
 assert abs(float(a[3])-float(b[3]))<1e-5,(a[:7],b[:7])
print('9159 original-machine-code wave cases matched: records, order, delay, marker wait, base and draw count')
