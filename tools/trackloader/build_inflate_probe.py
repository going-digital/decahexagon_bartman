#!/usr/bin/env python3
"""Build an isolated actual-68000 inflate harness from a local execram checkout."""
import argparse,subprocess
from pathlib import Path
p=argparse.ArgumentParser();p.add_argument('--execram-source',type=Path,required=True);p.add_argument('--unpatched', action='store_true');a=p.parse_args()
root=Path(__file__).resolve().parents[2];v=a.execram_source.resolve()/'src/musashi_vendor';o=root/'scratchpad/trackloader';o.mkdir(parents=True,exist_ok=True)
subprocess.run(['cc','-O2',str(v/'m68kmake.c'),'-o',str(o/'m68kmake')],check=True)
subprocess.run([str(o/'m68kmake'),str(o),str(v/'m68k_in.c')],check=True)
subprocess.run(['cc','-O2','-I'+str(v),'-I'+str(o),str(root/'tools/trackloader/inflate_probe.c'),str(v/'m68kcpu.c'),str(o/'m68kops.c'),str(v/'softfloat/softfloat.c'),'-lm','-o',str(o/'inflate_probe')],check=True)
core=a.execram_source.resolve()/'stubs/inflate/inflate_core.s'
if not a.unpatched:
 text=core.read_text()
 old='        sub.w   d0,a0'
 assert text.count(old)==1
 # SUBA.W sign-extends: legal distance 32768 would point forward.
 text=text.replace(old, '        and.l   #$ffff,d0\n        sub.l   d0,a0')
 core=o/'inflate_core_distance_fixed.s'
 core.write_text(text)
(o/'inflate_entry.s').write_text(f' bra.w inflate\n include "{core}"\n')
subprocess.run(['vasmm68k_mot','-m68000','-Fbin','-quiet','-o',str(o/'inflate.bin'),str(o/'inflate_entry.s')],check=True)
