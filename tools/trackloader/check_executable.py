#!/usr/bin/env python3
"""Compare in-place relocation with independent links; reject damaged packages."""
import ctypes,json,struct,subprocess,zlib,re
from pathlib import Path
from package_native_game import ROOT,elf_image
out=ROOT/'scratchpad/trackloader/native_game'
sdk=next((Path.home()/'.vscode/extensions').glob('bartmanabyss.amiga-debug-*/bin/darwin/opt/bin'))
libpath=out/'executable.dylib'
subprocess.run(['cc','-std=c99','-Wall','-Wextra','-Werror','-shared','-fPIC',str(ROOT/'trackloader/executable.c'),'-o',str(libpath)],check=True)
lib=ctypes.CDLL(str(libpath))
class Info(ctypes.Structure):_fields_=[('entry',ctypes.c_uint32),('memory_bytes',ctypes.c_uint32)]
fn=lib.track_executable_prepare
fn.argtypes=[ctypes.c_void_p,ctypes.c_uint32,ctypes.c_uint32,ctypes.c_uint32,ctypes.POINTER(Info)];fn.restype=ctypes.c_int
raw=(out/'game.exe1').read_bytes();memory=struct.unpack_from('>I',raw,12)[0];capacity=max(memory,len(raw))
sources=re.search(r'^c_sources := (.+)$',(ROOT/'Makefile').read_text(),re.M)[1].split()
sources+=['sfx.c','paula_irq.c','fib_pcm.c','pcm_lifecycle.c','tests/fib_stream.c','trackloader/chip_arena.c','trackloader/save.c','trackloader/save_disk.c','trackloader/native_save.c','trackloader/tune_cache.c']
objects=[str(out/(source.replace('/','_')+'.o')) for source in sources]+[str(out/'asm.o')]
rows=[]
for base in (0x20000,0x40000):
 elf=out/f'game_{base:x}.elf'
 subprocess.run([str(sdk/'m68k-amiga-elf-gcc'),'-m68000','-nostdlib',f'-Wl,--gc-sections,--undefined=fib_stream_bind,--undefined=fib_stream_unbind,--undefined=fib_stream_set_cue_lead,--undefined=track_save_select,-e,trackloader_game_entry,-Ttext={base:#x}',*objects,'-o',str(elf)],check=True)
 image,size,entry,_,address=elf_image(elf);assert address==base and size==memory
 expected=bytes(image)+bytes(memory-len(image));(out/f'expected_{base:x}.bin').write_bytes(expected)
 buf=ctypes.create_string_buffer(b'\xa5'*(capacity+32));ctypes.memmove(ctypes.addressof(buf)+16,raw,len(raw));info=Info()
 assert fn(ctypes.addressof(buf)+16,len(raw),capacity,base,ctypes.byref(info))
 assert buf.raw[16:16+memory]==expected and info.entry==base+entry and info.memory_bytes==memory
 assert buf.raw[:16]==b'\xa5'*16 and buf.raw[16+capacity:16+capacity+16]==b'\xa5'*16
 rows.append(dict(base=base,independent_link_matches=True,bss_zero=True,guards_intact=True))

def changed(offset,value):
 data=bytearray(raw);struct.pack_into('>I',data,offset,value);struct.pack_into('>I',data,24,0);struct.pack_into('>I',data,24,zlib.crc32(data));return bytes(data)
count=struct.unpack_from('>I',raw,20)[0]
cases=[('short_header',raw[:31],capacity,0x20000),('truncated',raw[:-1],capacity,0x20000),('trailing',raw+b'x',capacity,0x20000),('crc',raw[:-1]+bytes([raw[-1]^1]),capacity,0x20000),('magic',changed(0,0),capacity,0x20000),('header',changed(4,28),capacity,0x20000),('flags',changed(28,1),capacity,0x20000),('count',changed(20,0xffffffff),capacity,0x20000),('image',changed(8,0xffffffff),capacity,0x20000),('memory',changed(12,0xffffffff),capacity,0x20000),('entry',changed(16,1),capacity,0x20000),('entry_bounds',changed(16,memory),capacity,0x20000),('odd_fixup',changed(32,1),capacity,0x20000),('fixup_bounds',changed(32,memory),capacity,0x20000),('duplicate',changed(36,struct.unpack_from('>I',raw,32)[0]),capacity,0x20000),('target_bounds',changed(32+4*count+struct.unpack_from('>I',raw,32)[0],memory+1),capacity,0x20000),('capacity',raw,len(raw)-1,0x20000),('odd_base',raw,capacity,0x20001),('base_overflow',raw,capacity,0xffff0000)]
for name,data,cap,base in cases:
 buf=ctypes.create_string_buffer(b'\xa5'*(capacity+32));ctypes.memmove(ctypes.addressof(buf)+16,data,len(data));before=buf.raw;info=Info(0x12345678,0x87654321)
 assert not fn(ctypes.addressof(buf)+16,len(data),cap,base,ctypes.byref(info)),name
 assert buf.raw==before and info.entry==0x12345678 and info.memory_bytes==0x87654321,name
report=dict(status='Host actual-C relocation equals two independent m68k links',trials=rows,rejected_without_mutation=[c[0] for c in cases])
(ROOT/'docs/TRACKLOADER_EXECUTABLE_RESULTS.json').write_text(json.dumps(report,indent=2)+'\n')
print('Relocation matches independent links at two bases;',len(cases),'malformed cases rejected without writes')
