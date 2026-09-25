#!/usr/bin/env python3
"""Instruction-cycle estimate on Musashi 68000, without DMA contention."""
import argparse, json, subprocess
from pathlib import Path
root=Path(__file__).resolve().parents[1]
out=root/'scratchpad/ending_benchmark';out.mkdir(exist_ok=True)
sdk=next((Path.home()/'.vscode/extensions').glob('bartmanabyss.amiga-debug-*/bin/darwin/opt/bin'))
parser=argparse.ArgumentParser(description=__doc__)
parser.add_argument('--execram-source',type=Path,required=True)
parser.add_argument('--affine',action='store_true')
parser.add_argument('--incremental',action='store_true',help='Generate identical coordinates without per-point modulo')
parser.add_argument('--flat',action='store_true',help='Benchmark the production flat camera cache')
args=parser.parse_args()
if args.flat: args.affine=True
v=args.execram_source/'src/musashi_vendor'
entry=out/'entry.s';entry.write_text('.text\n.global _start\n_start: bra.w ending_projection_bench\n')
subprocess.run([str(sdk/'m68k-amiga-elf-gcc'),'-m68000',*(['-DFLAT_BENCH'] if args.flat else []),*(['-DINCREMENTAL_BENCH'] if args.incremental else []),'-O2','-ffreestanding','-fno-builtin','-nostdlib','-Wa,--register-prefix-optional','-Wl,-Ttext=0x1000',str(entry),*(['-DAFFINE_BENCH',str(root/'pc_ending_affine.c')] if args.affine else [str(root/'pc_ending_projection.c')]),str(root/'tests/ending_projection_bench.c'),str(root/'tests/ending_bench_arithmetic.c'),str(root/'support/gcc8_a_support.s'),'-o',str(out/'bench.elf')],check=True)
subprocess.run([str(sdk.parent/'m68k-amiga-elf/bin/objcopy'),'-O','binary',str(out/'bench.elf'),str(out/'bench.bin')],check=True)
subprocess.run(['cc','-O2','-I'+str(v),'-I'+str(root/'scratchpad/trackloader'),str(root/'tools/ending_projection_bench.c'),str(v/'m68kcpu.c'),str(root/'scratchpad/trackloader/m68kops.c'),str(v/'softfloat/softfloat.c'),'-lm','-o',str(out/'probe')],check=True)
rows=[]
for n in (0,4,40,100,200):
 r=json.loads(subprocess.check_output([str(out/'probe'),str(out/'bench.bin'),str(n)],text=True,timeout=60))
 r['milliseconds_at_7mhz']=r['cycles']/7000
 rows.append(r);print(r,flush=True)
(root/('docs/ENDING_FLAT_BENCHMARK.json' if args.flat else (('docs/ENDING_AFFINE_INCREMENTAL_BENCHMARK.json' if args.incremental else 'docs/ENDING_AFFINE_BENCHMARK.json') if args.affine else ('docs/ENDING_PROJECTION_INCREMENTAL_BENCHMARK.json' if args.incremental else 'docs/ENDING_PROJECTION_BENCHMARK.json')))).write_text(json.dumps({'flat_camera':args.flat,'coordinate_generator':'incremental' if args.incremental else 'modulo','scope':'68000 instruction cycles, projection only; no DMA, clipping, drawing or game update','compiler_flags':('-m68000 -O2; affine setup once; no LTO' if args.affine else '-m68000 -O2; benchmark shift/add and restoring-division helpers; no LTO'),'trials':rows},indent=2)+'\n')
