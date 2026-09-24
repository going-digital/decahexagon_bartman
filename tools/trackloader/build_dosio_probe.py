#!/usr/bin/env python3
"""Build the original 68000 OFS image writer without creating a trial disk."""
import argparse
import subprocess
from pathlib import Path
p=argparse.ArgumentParser(description=__doc__)
p.add_argument('--execram-source',type=Path,required=True)
a=p.parse_args()
root=Path(__file__).resolve().parents[2]
out=root/'scratchpad/trackloader'
v=a.execram_source.resolve()/'src/musashi_vendor'
if not (out/'m68kops.c').is_file():
    p.error('Run build_inflate_probe.py first to generate Musashi opcode sources')
subprocess.run(['cc','-O2','-I'+str(v),'-I'+str(out),
                str(root/'tools/trackloader/dosio_probe.c'),str(v/'m68kcpu.c'),
                str(out/'m68kops.c'),str(v/'softfloat/softfloat.c'),'-lm',
                '-o',str(out/'dosio_probe')],check=True)
subprocess.run(['vasmm68k_mot','-m68000','-Fbin','-quiet','-I'+str(root),
                '-o',str(out/'dosio.bin'),str(root/'DosIO.s')],check=True)
