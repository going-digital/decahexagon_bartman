#!/usr/bin/env python3
"""Check actual 68000 DosIO/DiskIO failure returns with mocked hardware inputs."""
import argparse,json,subprocess
from pathlib import Path
p=argparse.ArgumentParser();p.add_argument('--execram-source',type=Path,required=True);a=p.parse_args()
root=Path(__file__).resolve().parents[2];out=root/'scratchpad/trackloader';v=a.execram_source/'src/musashi_vendor'
subprocess.run(['vasmm68k_mot','-m68000','-Fbin','-quiet','-I'+str(root),'-o',str(out/'dosio_failure.bin'),str(root/'DosIO.s')],check=True)
subprocess.run(['cc','-O2','-I'+str(v),'-I'+str(out),str(root/'tools/trackloader/disk_failure_probe.c'),str(v/'m68kcpu.c'),str(out/'m68kops.c'),str(v/'softfloat/softfloat.c'),'-lm','-o',str(out/'disk_failure_probe')],check=True)
result=subprocess.check_output([str(out/'disk_failure_probe'),str(out/'dosio_failure.bin')],text=True,timeout=30)
report=dict(status='Unmodified 68000 DosIO/DiskIO failure paths return with destination intact',hardware='Mock CIA/custom inputs; timers expire immediately; not wall-clock timing or full emulator no-media validation',scenarios=['no disk','DMA never completes','track-zero never asserts'],trials=[json.loads(line) for line in result.splitlines()])
(root/'docs/TRACKLOADER_DISK_FAILURE_RESULTS.json').write_text(json.dumps(report,indent=2)+'\n');print(result)
