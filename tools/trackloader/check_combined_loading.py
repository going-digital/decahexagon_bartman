#!/usr/bin/env python3
"""Run actual 68000 inflate then C Fibonacci expansion in the same arena."""
import hashlib,json,subprocess,zlib
from pathlib import Path
root=Path(__file__).resolve().parents[2];out=root/'scratchpad/trackloader'
sdk=next((Path.home()/'.vscode/extensions').glob('bartmanabyss.amiga-debug-*/bin/darwin/opt/bin'))
cc=sdk/'m68k-amiga-elf-gcc';objcopy=sdk.parent/'m68k-amiga-elf/bin/objcopy'
# First text fragment is an absolute JMP; C arguments are already on the stack.
entry=out/'fib_entry.s';entry.write_text('.text\n.global _start\n_start:\n jmp trackloader_fib_expand\n')
subprocess.run([str(cc),'-m68000','-O2','-nostdlib','-fomit-frame-pointer','-fno-builtin','-Wl,-Ttext=0x10000',str(entry),str(root/'trackloader/fib_expand.c'),str(root/'fib_decode.c'),'-o',str(out/'fib_expand.elf')],check=True)
subprocess.run([str(objcopy),'-O','binary',str(out/'fib_expand.elf'),str(out/'fib_expand.bin')],check=True)
rows=[]
for name in ['courtesy','focus','otis']:
 packed=out/f'{name}.fibonacci.deflate';expected=out/f'{name}.fibonacci.expected'
 expected.write_bytes(zlib.decompress(packed.read_bytes(),-15))
 final=out/f'{name}.pcm.expected';final.write_bytes(zlib.decompress((out/f'{name}.pcm.deflate').read_bytes(),-15))
 r=json.loads(subprocess.check_output([str(out/'inflate_probe'),str(out/'inflate.bin'),str(packed),str(expected),str(out/'fib_expand.bin'),str(final)],text=True))
 r['track']=name;r['cpu_seconds_pal_lower_bound']=(r['inflate_cycles']+r['expansion_cycles'])/7093790
 rows.append(r);print(r,flush=True)
(root/'docs/TRACKLOADER_COMBINED_RESULTS.json').write_text(json.dumps({'status':'Actual 68000 execution, shared arena, guards and final bytes checked; excludes disk/chipset timing','expansion_binary_sha256':hashlib.sha256((out/'fib_expand.bin').read_bytes()).hexdigest(),'trials':rows},indent=2)+'\n')
