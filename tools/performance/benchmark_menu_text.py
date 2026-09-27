#!/usr/bin/env python3
"""68000 menu text benchmark, excluding clear/copy DMA and cache effects."""
from pathlib import Path
import subprocess,json
root=Path(__file__).resolve().parents[2];out=root/'scratchpad/menu_blits';out.mkdir(exist_ok=True)
sdk=next((Path.home()/'.vscode/extensions').glob('bartmanabyss.amiga-debug-*/bin/darwin/opt/bin'))
probe=root/'scratchpad/performance_row/probe'
(out/'entry.s').write_text('.text\n.global _start\n_start: bra.w bench\n')
rows=[]
for variant,path in [('before',root/'tests/fixtures/menu_raster_reference.c'),('after',root/'hud.c')]:
 s=path.read_text();s=s[s.index('static const UBYTE front_font'):s.index('void hud_draw_front')]
 code='''#include "menu_font16.h"
typedef unsigned char UBYTE;typedef unsigned short UWORD;
#define SCREEN_WIDTH 320
#define SCREEN_WIDTH_BYTES 40
#pragma GCC optimize ("Os")
unsigned strlen(const char *s){const volatile char *p=s;unsigned n=0;while(p[n])++n;return n;}
'''+s+'''static UBYTE plane[8000];
unsigned bench(unsigned count){
 for(unsigned i=0;i<count;++i){
 front_text(plane,"HEXAGON",32,2);front_text(plane,"OPTIONS",70,2);
 front_text(plane,"ORIGINAL GAME CONCEPT AND DESIGN",110,1);
 front_text(plane,"CHIPZELMUSIC.BANDCAMP.COM",130,1);
 }return count;}
'''
 (out/'bench.c').write_text(code)
 subprocess.run([str(sdk/'m68k-amiga-elf-gcc'),'-m68000','-O2','-ffreestanding','-fno-builtin','-fomit-frame-pointer','-nostdlib','-I'+str(root),'-Wa,--register-prefix-optional','-Wl,-Ttext=0x1000',str(out/'entry.s'),str(out/'bench.c'),str(root/'support/gcc8_a_support.s'),'-o',str(out/'bench.elf')],check=True)
 subprocess.run([str(sdk.parent/'m68k-amiga-elf/bin/objcopy'),'-O','binary',str(out/'bench.elf'),str(out/'bench.bin')],check=True)
 def run(n):return json.loads(subprocess.check_output([str(probe),str(out/'bench.bin'),str(n)],text=True,timeout=30))['cycles']
 cycles=run(10)-run(0);rows.append(dict(variant=variant,cycles_per_four_lines=cycles/10))
r=dict(scope='Musashi 68000 menu text only, two 16x16 headings and two small credit lines; no clear/copy DMA or cache effects, identical loop overhead. Not a whole transition timing.',trials=rows)
(root/'docs/MENU_TEXT_BENCHMARK.json').write_text(json.dumps(r,indent=2)+'\n');print(r)
