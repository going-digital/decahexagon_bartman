#!/usr/bin/env python3
"""Extract one original-machine-code case per implemented ID for target checks."""
import gzip, math
from pathlib import Path
root = Path(__file__).resolve().parent.parent
inputs = (root/'tests/fixtures/pc_wave_inputs.txt').read_text().splitlines()
outputs = gzip.open(root/'tests/fixtures/pc_wave_native.txt.gz', 'rt').read().splitlines()
rows = []
seen = set()
for inp, out in zip(inputs, outputs):
    wave, speed, count, *rolls = map(int, inp.split())
    if wave in seen:
        continue
    seen.add(wave)
    fields = out.split()
    digest = 2166136261
    for record in fields[7:]:
        slot, distance, width, unused, active = map(int, record.split(','))
        for value in (slot, distance, width, active):
            digest = ((digest ^ value) * 16777619) & 0xffffffff
    rows.append('{%d,%s,%d,%s,%s,%s,%d,0x%08xu,{%s}}' % (
        wave, fields[2], math.ceil(float(fields[3])), fields[4], fields[5], fields[6], count,
        digest, ','.join(map(str, rolls))))
(root/'tests/wave_cases.inc').write_text('/* Generated from original x86 execution, tools/build_wave_checks.py. */\n' + ',\n'.join(rows) + '\n')
