#!/usr/bin/env python3
"""Reproduce the original driver's seek-time change-latch blind spot."""
import argparse
import json
import subprocess
from pathlib import Path

parser = argparse.ArgumentParser()
parser.add_argument('--execram-source', type=Path, required=True)
args = parser.parse_args()
root = Path(__file__).resolve().parents[2]
out = root / 'scratchpad/trackloader'
vendor = args.execram_source / 'src/musashi_vendor'
subprocess.run(['vasmm68k_mot', '-m68000', '-Fbin', '-quiet', '-o',
                str(out / 'disk_change.bin'), str(root / 'DiskIO.s')], check=True)
subprocess.run(['cc', '-O2', '-I' + str(vendor), '-I' + str(out),
                str(root / 'tools/trackloader/disk_change_probe.c'),
                str(vendor / 'm68kcpu.c'), str(out / 'm68kops.c'),
                str(vendor / 'softfloat/softfloat.c'), '-lm', '-o',
                str(out / 'disk_change_probe')], check=True)
result = subprocess.check_output([str(out / 'disk_change_probe'),
                                  str(out / 'disk_change.bin')], text=True, timeout=30)
report = {
    'status': 'Known gap reproduced: seek-cleared change is indistinguishable from no change',
    'scope': 'Original 68000 DiskIO; modeled CIA latch, immediate timers, no valid MFM input',
    'scenarios': ['no change', 'replacement after wrapper check; latch cleared by step',
                  'change latch remains asserted'],
    'trials': [json.loads(line) for line in result.splitlines()],
    'limitation': 'Read DMA reached; replacement-media write not demonstrated. Physical timing untested.',
}
(root / 'docs/TRACKLOADER_DISK_CHANGE_RESULTS.json').write_text(json.dumps(report, indent=2) + '\n')
print(result)

subprocess.run(['python3', str(root / 'tools/trackloader/make_guarded_diskio.py')], check=True)
subprocess.run(['vasmm68k_mot', '-m68000', '-Fbin', '-quiet', '-o',
                str(out / 'guarded_diskio.bin'), str(out / 'native_game/guarded_diskio.s')], check=True)
guarded = subprocess.check_output([str(out / 'disk_change_probe'),
                                  str(out / 'guarded_diskio.bin'), 'guarded'], text=True, timeout=30)
report['guarded_trials'] = [json.loads(line) for line in guarded.splitlines()]
report['guarded_scope'] = 'Seek-time model aborts before stepping or DMA; write guards require further MFM/timing validation'
(root / 'docs/TRACKLOADER_DISK_CHANGE_RESULTS.json').write_text(json.dumps(report, indent=2) + '\n')
print(guarded)
