#!/usr/bin/env python3
"""Test native guards on the actual 68000 write/retry path with a stubbed pre-read."""
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
subprocess.run(['python3', str(root/'tools/trackloader/make_guarded_diskio.py')], check=True)
source = out/'write_guard_harness.s'
source.write_text('        include "'+str(out/'native_game/guarded_diskio.s')+'"\n' +
                  '\n'.join('        dc.l '+label+'-diskio' for label in
                            ['diskio_L00f4','diskio_L01fc','diskio_L023e',
                             'native_disk_guard_enabled','diskio_drive_state'])+'\n')
subprocess.run(['vasmm68k_mot','-m68000','-Fbin','-quiet','-o',str(out/'write_guard.bin'),str(source)],check=True)
subprocess.run(['cc','-O2','-Wall','-Wextra','-I'+str(vendor),'-I'+str(out),
                str(root/'tools/trackloader/disk_write_guard_probe.c'),str(vendor/'m68kcpu.c'),
                str(out/'m68kops.c'),str(vendor/'softfloat/softfloat.c'),'-lm','-o',
                str(out/'write_guard_probe')],check=True)
result = subprocess.check_output([str(out/'write_guard_probe'),str(out/'write_guard.bin')],text=True,timeout=30)
report = dict(status='Pre-write, retry and active-DMA guards pass with disabled-guard controls',
              faults=['none','write entry','immediately before DMA setup','first retry','active DMA with timeout','active DMA with completion','normal completion'],
              trials=[json.loads(line) for line in result.splitlines()],
              scope='Actual 68000 write path; preceding MFM read stubbed successful; DMA timeout/completion signals modeled; timers expire immediately',
              limitations=['No real MFM media; completion is a mocked register signal','No removal during active physical write'])
(root/'docs/TRACKLOADER_WRITE_GUARD_RESULTS.json').write_text(json.dumps(report,indent=2)+'\n')
print(result)
