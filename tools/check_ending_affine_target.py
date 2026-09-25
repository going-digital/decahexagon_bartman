#!/usr/bin/env python3
"""Link the actual affine drawing pipeline for 68000; no hardware execution."""
from pathlib import Path
import json
import subprocess
root=Path(__file__).resolve().parents[1]
out=root/'scratchpad/ending_affine_target';out.mkdir(exist_ok=True)
sdk=next((Path.home()/'.vscode/extensions').glob('bartmanabyss.amiga-debug-*/bin/darwin/opt/bin'))
cc=str(sdk/'m68k-amiga-elf-gcc')
entry=out/'entry.c'
entry.write_text('#include "hw.h"\nstruct Custom *custom=(struct Custom*)0xdff000;\nvoid ending_link_entry(void) {}\n')
sources=['pc_ending_affine.c','pc_ending_wall.c','pc_ending_scene.c','pc_projection.c','player_shape.c','render_ending_affine.c','blitter.c','render_clip.c']
objects=[]
for source in [*(root/s for s in sources),entry]:
 obj=out/(source.stem+'.o');objects.append(str(obj))
 subprocess.run([cc,'-m68000','-O2','-ffreestanding','-fno-builtin','-ffunction-sections','-fdata-sections','-DBUILD_DEBUG=0','-DTRACKLOADER=1','-Wno-volatile-register-var','-I'+str(root),'-c',str(source),'-o',str(obj)],check=True)
asm=out/'support.o';objects.append(str(asm))
subprocess.run([cc,'-m68000','-Wa,--register-prefix-optional','-c',str(root/'support/gcc8_a_support.s'),'-o',str(asm)],check=True)
required=['render_ending_flat_frame','render_ending_affine_walls','render_ending_affine_hub','render_ending_affine_player','blit_cls','blit_fill_reset','blit_line_mode','blit_fill']
elf=out/'ending.elf'
subprocess.run([cc,'-m68000','-nostdlib','-Wl,--gc-sections,-e,ending_link_entry,-Ttext=0x1000',*['-Wl,--undefined='+s for s in required],*objects,'-o',str(elf)],check=True)
nm=str(sdk.parent/'m68k-amiga-elf/bin/nm')
undefined=subprocess.check_output([nm,'-u',str(elf)],text=True).strip();assert not undefined,undefined
symbols=subprocess.check_output([nm,str(elf)],text=True)
names={line.split()[-1] for line in symbols.splitlines()}
assert all(s in names for s in required)
for forbidden in ['__muldi3','__divdi3','__udivdi3','pc_ending_project_wide','pc_ending_clip_quad','pc_ending_affine_prepare','sine','sn']:
 assert forbidden not in names,forbidden
sizes=subprocess.check_output([str(sdk.parent/'m68k-amiga-elf/bin/objdump'),'-h',str(elf)],text=True)
report={'status':'Actual affine scene and blitter path links for 68000; not executed',
        'required_symbols':required,'undefined_symbols':[], 'sources':sources,'sizes':sizes,
        'limitations':['No emulator/hardware frame execution','No complete-frame cycle measurement','Game loop and ending lifecycle not connected']}
(root/'docs/ENDING_AFFINE_TARGET_RESULTS.json').write_text(json.dumps(report,indent=2)+'\n')
print(sizes.strip());print('Affine pipeline links without 64-bit arithmetic or perspective helpers')
