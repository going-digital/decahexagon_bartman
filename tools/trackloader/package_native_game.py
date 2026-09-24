#!/usr/bin/env python3
"""Package the linked big-endian 68000 ELF as validated EXE1 + raw Zultra."""
import ctypes,hashlib,json,struct,zlib
from pathlib import Path
ROOT=Path(__file__).resolve().parents[2]
def elf_image(path):
 data=Path(path).read_bytes()
 assert data[:7]==b'\x7fELF\x01\x02\x01'
 h=struct.unpack_from('>HHIIIIIHHHHHH',data,16)
 assert h[0]==2 and h[1]==4 and h[10]==40
 entry,table,count=h[3],h[5],h[11]
 sections=[struct.unpack_from('>10I',data,table+40*i) for i in range(count)]
 allocated=[s for s in sections if s[2]&2 and s[5]]
 base=min(s[3] for s in allocated)
 end=max(s[3]+s[5] for s in allocated)
 loaded=max(s[3]+s[5] for s in allocated if s[1]!=8)
 image=bytearray(loaded-base)
 occupied=[]
 for s in allocated:
  assert s[1] in (1,8),s
  assert all(s[3]+s[5]<=a or s[3]>=b for a,b in occupied)
  occupied.append((s[3],s[3]+s[5]))
  if s[1]!=8:
   assert s[4]+s[5]<=len(data)
   image[s[3]-base:s[3]-base+s[5]]=data[s[4]:s[4]+s[5]]
  else: assert s[3]>=loaded,'interleaved BSS not supported'
 reloc=[]
 for s in sections:
  if s[1] not in (4,9):continue
  target=sections[s[7]]
  if not target[2]&2:continue
  assert s[1]==4 and s[9]==12,'expected RELA'
  symbols=sections[s[6]];assert symbols[9]==16
  for at in range(s[4],s[4]+s[5],12):
   offset,info,addend=struct.unpack_from('>IIi',data,at)
   kind=info&255
   if kind==0:continue
   sym=struct.unpack_from('>IIIBBH',data,symbols[4]+16*(info>>8))
   index=sym[5]
   assert index and (index==0xfff1 or index<len(sections))
   if index==0xfff1:
    assert kind==1,'PC-relative absolute symbol unsupported'
    continue
   assert sections[index][2]&2
   assert kind in (1,4,5,6),kind
   if kind!=1:continue # same-image PC-relative addresses do not move
   assert offset%2==0 and target[3]<=offset and offset+4<=target[3]+target[5]
   reloc.append(offset-base)
 assert len(reloc)==len(set(reloc))
 return image,end-base,entry-base,sorted(reloc),base

def package(path):
 image,memory,entry,reloc,base=elf_image(path)
 assert base==0 and entry<len(image) and entry%2==0
 assert all(struct.unpack_from('>I',image,r)[0]<=memory for r in reloc)
 body=struct.pack('>8I',0x45584531,32,len(image),memory,entry,len(reloc),0,0)
 body+=b''.join(struct.pack('>I',r) for r in reloc)+image
 body=bytearray(body);struct.pack_into('>I',body,24,zlib.crc32(body))
 return bytes(body)

def compress(raw,library):
 lib=ctypes.CDLL(str(library))
 lib.zultra_memory_bound.argtypes=[ctypes.c_size_t,ctypes.c_uint,ctypes.c_uint];lib.zultra_memory_bound.restype=ctypes.c_size_t
 lib.zultra_memory_compress.argtypes=[ctypes.c_void_p,ctypes.c_size_t,ctypes.c_void_p,ctypes.c_size_t,ctypes.c_uint,ctypes.c_uint];lib.zultra_memory_compress.restype=ctypes.c_size_t
 dst=ctypes.create_string_buffer(lib.zultra_memory_bound(len(raw),0,0))
 size=lib.zultra_memory_compress(raw,len(raw),dst,len(dst),0,0)
 assert 0<size<=len(dst)
 packed=dst.raw[:size];assert zlib.decompress(packed,-15)==raw
 return packed
if __name__=='__main__':
 out=ROOT/'scratchpad/trackloader';raw=package(out/'native_game/game.elf')
 packed=compress(raw,out/'libzultra.dylib')
 (out/'native_game/game.exe1').write_bytes(raw);(out/'native_game/game.deflate').write_bytes(packed)
 report=dict(elf_sha256=hashlib.sha256((out/'native_game/game.elf').read_bytes()).hexdigest(),package_bytes=len(raw),packed_bytes=len(packed),image_bytes=struct.unpack_from('>I',raw,8)[0],memory_bytes=struct.unpack_from('>I',raw,12)[0],relocations=struct.unpack_from('>I',raw,20)[0],sha256=hashlib.sha256(raw).hexdigest(),packed_sha256=hashlib.sha256(packed).hexdigest())
 (ROOT/'docs/TRACKLOADER_EXECUTABLE_PACKAGE_RESULTS.json').write_text(json.dumps(report,indent=2)+'\n');print(report)
