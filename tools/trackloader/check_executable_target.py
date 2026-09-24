#!/usr/bin/env python3
"""Run the executable preparer on Musashi against independent linked images."""
import argparse,hashlib,json,subprocess
from pathlib import Path
p=argparse.ArgumentParser();p.add_argument('--execram-source',type=Path,required=True);args=p.parse_args()
root=Path(__file__).resolve().parents[2];out=root/'scratchpad/trackloader';native=out/'native_game';v=args.execram_source/'src/musashi_vendor'
sdk=next((Path.home()/'.vscode/extensions').glob('bartmanabyss.amiga-debug-*/bin/darwin/opt/bin'))
entry=native/'executable_entry.s';entry.write_text('.text\n.global _start\n_start: bra.w track_executable_prepare\n')
subprocess.run([str(sdk/'m68k-amiga-elf-gcc'),'-m68000','-mpcrel','-Wa,--register-prefix-optional','-O2','-ffreestanding','-nostdlib','-fno-builtin','-Wl,-Ttext=0x1000',str(entry),str(root/'trackloader/executable.c'),'-o',str(native/'executable.elf')],check=True)
subprocess.run([str(sdk.parent/'m68k-amiga-elf/bin/objcopy'),'-O','binary',str(native/'executable.elf'),str(native/'executable.bin')],check=True)
probe=native/'executable_probe'
subprocess.run(['cc','-O2','-I'+str(v),'-I'+str(out),str(root/'tools/trackloader/executable_probe.c'),str(v/'m68kcpu.c'),str(out/'m68kops.c'),str(v/'softfloat/softfloat.c'),'-lm','-o',str(probe)],check=True)
rows=[]
for base in (0x20000,0x40000):
 result=subprocess.check_output([str(probe),str(native/'executable.bin'),str(native/'game.exe1'),str(native/f'expected_{base:x}.bin'),hex(base)],text=True,timeout=90)
 rows.append(json.loads(result));print(result.strip(),flush=True)
inflate=json.loads(subprocess.check_output([str(out/'inflate_probe'),str(out/'inflate.bin'),str(native/'game.deflate'),str(native/'game.exe1')],text=True,timeout=90))
inflate['packed_sha256']=hashlib.sha256((native/'game.deflate').read_bytes()).hexdigest()
(root/'docs/TRACKLOADER_EXECUTABLE_TARGET_RESULTS.json').write_text(json.dumps(dict(status='Actual 68000 relocation matches independent links; not full-game boot',trials=rows,inflate=inflate),indent=2)+'\n')
