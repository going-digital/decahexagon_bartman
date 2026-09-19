#!/usr/bin/env python3
"""Native ordered opening/handoff traces; delay is ceil(float) at dt=1."""
import gzip
import math
from pathlib import Path
import subprocess
root=Path(__file__).resolve().parents[1]
with gzip.open(root/'tests/fixtures/pc_progression_native.txt.gz','rt') as f:
    expected=f.read().splitlines()
actual=subprocess.check_output([str(root/'out/progression_probe')],text=True).splitlines()
assert len(expected)==len(actual)
for e,a in zip(expected,actual):
    fields=e.split()
    fields[9]=str(math.ceil(float(fields[9])))
    assert ' '.join(fields)==a,(e,a)
print(f'{len(expected):,} native opening/handoff ticks match, including RNG and wall hashes')
