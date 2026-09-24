#!/usr/bin/env python3
import struct,subprocess
from pathlib import Path
root=Path(__file__).resolve().parents[2];out=root/'scratchpad/trackloader'
for name in ['disk_boot','disk_stage']:
 args=['vasmm68k_mot','-m68000','-Fbin','-quiet','-I'+str(root)]
 if name=='disk_stage':args+=['-DFOCUS_SUM='+str(sum((out/'focus.fibonacci.deflate').read_bytes()))]
 subprocess.run(args+['-o',str(out/(name+'.bin')),str(root/'trackloader/tests'/ (name+'.s'))],check=True)
data=bytearray((out/'reserved_layout.adf').read_bytes())
stage=(out/'disk_stage.bin').read_bytes();assert len(stage)<=22528
data[1661*512:1661*512+len(stage)]=stage
boot=(out/'disk_boot.bin').read_bytes();assert len(boot)<=1024
block=bytearray(1024);block[:len(boot)]=boot
s=0
for word in struct.unpack('>256I',block):
 s+=word;s=(s&0xffffffff)+(s>>32)
struct.pack_into('>I',block,4,(~s)&0xffffffff)
data[:1024]=block
(out/'disk_dma_trial.adf').write_bytes(data)
print('Diagnostic disk built; stage bytes:',len(stage))
