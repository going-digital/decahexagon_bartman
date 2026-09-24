#!/usr/bin/env python3
"""Link a resident game candidate; audit imports, not a bootable image builder."""
import json,re,subprocess
from pathlib import Path
root=Path(__file__).resolve().parents[2]
out=root/'scratchpad/trackloader/native_game';out.mkdir(parents=True,exist_ok=True)
sdk=next((Path.home()/'.vscode/extensions').glob('bartmanabyss.amiga-debug-*/bin/darwin/opt/bin'))
binutils=sdk.parent/'m68k-amiga-elf/bin'
sources=re.search(r'^c_sources := (.+)$',(root/'Makefile').read_text(),re.M)[1].split()
sources+=['sfx.c','paula_irq.c','fib_pcm.c','pcm_lifecycle.c','tests/fib_stream.c','trackloader/chip_arena.c','trackloader/save.c','trackloader/save_disk.c','trackloader/native_save.c']
flags=['-m68000','-O2','-ffreestanding','-fno-builtin','-fomit-frame-pointer','-ffunction-sections','-fdata-sections','-DBUILD_DEBUG=0','-DTRACKLOADER=1','-DMUSIC_FIB_STREAM=1','-DPCM_EXTERNAL_ONLY=1','-DSOUND_EFFECTS=1','-DCHEAT_MODE=0','-Wno-volatile-register-var']
# Storage transactions run only while loading/saving, not in the frame or IRQ path.
size_sources={'trackloader/save.c','trackloader/save_disk.c','trackloader/native_save.c'}
objects=[]
for source in sources:
 obj=out/(source.replace('/','_')+'.o');objects.append(str(obj))
 subprocess.run([str(sdk/'m68k-amiga-elf-gcc'),*flags,*(['-Os'] if source in size_sources else []),'-c',str(root/source),'-o',str(obj)],cwd=root,check=True)
obj=out/'asm.o';objects.append(str(obj))
subprocess.run([str(sdk/'m68k-amiga-elf-gcc'),'-m68000','-Wa,--register-prefix-optional','-c',str(root/'support/gcc8_a_support.s'),'-o',str(obj)],check=True)
elf=out/'game.elf'
subprocess.run([str(sdk/'m68k-amiga-elf-gcc'),'-m68000','-nostdlib','-Wl,--emit-relocs,--gc-sections,--undefined=fib_stream_bind,--undefined=fib_stream_unbind,--undefined=fib_stream_set_cue_lead,--undefined=track_save_select,-e,trackloader_game_entry,-Ttext=0',*objects,'-o',str(elf)],check=True)
undefined=subprocess.check_output([str(binutils/'nm'),'-u',str(elf)],text=True).strip()
assert not undefined,undefined
symbols=subprocess.check_output([str(binutils/'nm'),str(elf)],text=True)
for forbidden in ['SysBase','DOSBase','GfxBase','_start','main','KPrintF']:
 assert not re.search(r'\b'+forbidden+r'$',symbols,re.M),forbidden
sections=subprocess.check_output([str(binutils/'objdump'),'-h',str(elf)],text=True)
report=dict(status='Native game links without OS base symbols or undefined imports; not boot tested',entry='trackloader_game_entry',music='external-only',sound_effects=True,sections=sections,limitations=['No resident executable load/relocation yet','Static DMA sections require Chip placement','No native target boot or gameplay validation','68000 only'])
(root/'docs/TRACKLOADER_NATIVE_GAME_RESULTS.json').write_text(json.dumps(report,indent=2)+'\n')
print(sections)
print('Native game linked; OS bases and unresolved imports absent')
