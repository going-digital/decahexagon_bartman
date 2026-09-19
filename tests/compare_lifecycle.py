#!/usr/bin/env python3
"""Compare bounded PC lifecycle, unlock and record traces with portable code."""
from pathlib import Path
import gzip
import subprocess
root=Path(__file__).resolve().parents[1]
expected=gzip.decompress((root/'tests/fixtures/pc_lifecycle_native.txt.gz').read_bytes()).decode().splitlines()
actual=subprocess.check_output([str(root/'out/lifecycle_probe')],text=True).splitlines()
assert len(actual)==len(expected), (len(actual),len(expected))
for i,(a,b) in enumerate(zip(actual,expected)):
    assert a==b, f'row {i}: portable {a}; native {b}'
print(f'{len(actual):,} PC lifecycle cases match: start, retry gate, frozen score and angle preservation')
