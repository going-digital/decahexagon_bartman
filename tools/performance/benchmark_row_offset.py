#!/usr/bin/env python3
"""Run after check_row_offset.py has built the before object and Musashi probe."""
from pathlib import Path
import subprocess,json
root=Path(__file__).resolve().parents[2];out=root/'scratchpad/performance_row';sdk=next((Path.home()/'.vscode/extensions').glob('bartmanabyss.amiga-debug-*/bin/darwin/opt/bin'))
rows=[]
for variant,fn in [('before','old_blit_clipped_line_onedot'),('after','blit_clipped_line_onedot')]:
 (out/'time.c').write_text('''#include "blitter.h"
struct Custom *custom;
void old_blit_clipped_line_onedot(WORD,WORD,WORD,WORD,UWORD,void*);
unsigned bench(unsigned count){custom=(void*)0x180000;
 for(unsigned n=0;n<count;++n)for(unsigned y=0;y<199;++y){CALL(0,y,319,y+1,0,(void*)0x120000);CALL(319,y+1,0,y,0,(void*)0x120000);}
 return count;}
'''.replace('CALL',fn))
 subprocess.run([str(sdk/'m68k-amiga-elf-gcc'),'-m68000','-O2','-DBUILD_DEBUG=0','-ffreestanding','-fno-builtin','-fomit-frame-pointer','-ffunction-sections','-fdata-sections','-I'+str(root),'-nostdlib','-Wa,--register-prefix-optional','-Wl,--gc-sections,-Ttext=0x1000',str(out/'entry.s'),str(out/'time.c'),str(out/'before.o'),str(root/'blitter.c'),str(root/'render_clip.c'),str(root/'support/gcc8_a_support.s'),'-o',str(out/'time.elf')],check=True)
 subprocess.run([str(sdk.parent/'m68k-amiga-elf/bin/objcopy'),'-O','binary',str(out/'time.elf'),str(out/'time.bin')],check=True)
 def run(n):return json.loads(subprocess.check_output([str(out/'probe'),str(out/'time.bin'),str(n)],text=True))['cycles']
 total=run(10)-run(0);rows.append(dict(variant=variant,cycles=total,calls=3980,cycles_per_call=total/3980))
r=dict(scope='Musashi 68000 complete clipped-wrapper calls, both endpoint orders across all on-screen starting rows; identical loop overhead, inert registers, no DMA/IRQs. Not a representative mix of offscreen lines.',trials=rows)
(root/'docs/ROW_OFFSET_BENCHMARK.json').write_text(json.dumps(r,indent=2)+'\n');print(r)
