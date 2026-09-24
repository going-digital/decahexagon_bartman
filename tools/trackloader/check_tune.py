#!/usr/bin/env python3
"""Validate loaded packages and play through the real PCM sequencer."""
import ctypes as C,json,struct,subprocess,zlib
from pathlib import Path
root=Path(__file__).resolve().parents[2];out=root/'scratchpad/trackloader'
subprocess.run(['cc','-O2','-Wall','-Wextra','-Werror','-shared','-fPIC',*[str(root/p) for p in ['trackloader/tune.c','trackloader/fib_expand.c','fib_decode.c','fib_pcm.c']],'-o',str(out/'tune.dylib')],check=True)
class Song(C.Structure):
 _fields_=[(n,C.c_void_p) for n in ['sequence','offsets','bank','edges','raw']]+[('split',C.c_uint)]+[(n,C.c_ushort) for n in ['count','index','left']]
class Info(C.Structure):
 _fields_=[(n,C.c_uint) for n in ['rate','samples','meta','pcm','arena']]
lib=C.CDLL(str(out/'tune.dylib'));fn=lib.trackloader_tune_prepare
fn.argtypes=[C.c_void_p,C.c_uint,C.c_uint,C.POINTER(Song),C.POINTER(Info)];fn.restype=C.c_int
lib.fib_song_read.argtypes=[C.POINTER(Song),C.c_void_p,C.c_uint]
lib.fib_song_seek.argtypes=[C.POINTER(Song),C.c_uint];lib.fib_song_seek.restype=C.c_int
rows=[]
for name in ['courtesy','focus','otis']:
 raw=zlib.decompress((out/f'{name}.fibonacci.deflate').read_bytes(),-15)
 final=zlib.decompress((out/f'{name}.pcm.deflate').read_bytes(),-15)
 buf=C.create_string_buffer(len(final)+32);C.memset(buf,0xa5,len(buf));C.memmove(C.byref(buf,16),raw,len(raw))
 song=Song();info=Info();assert fn(C.byref(buf,16),len(raw),len(final),C.byref(song),C.byref(info))
 assert buf.raw[16:16+len(final)]==final
 banks,count=struct.unpack_from('>II',final,12);offsets=struct.unpack_from('>'+str(banks+1)+'I',final,24)
 seq=struct.iter_unpack('>HH',final[24+4*(banks+1):info.meta])
 expected=b''.join(final[info.meta+offsets[i]:info.meta+offsets[i]+n] for i,n in seq)
 want=expected+expected[:4096];got=bytearray()
 while len(got)<len(want):
  n=min(511,len(want)-len(got));dst=C.create_string_buffer(n);lib.fib_song_read(C.byref(song),dst,n);got+=dst.raw
 assert got==want and buf.raw[:16]==buf.raw[-16:]==b'\xa5'*16
 for pos in [0,1,len(expected)//2,len(expected)-1]:
  assert lib.fib_song_seek(C.byref(song),pos);dst=C.create_string_buffer(513);lib.fib_song_read(C.byref(song),dst,513)
  assert dst.raw==(expected+expected)[pos:pos+513]
 # Header, offsets, sequence, FIB header, truncation and capacity failures.
 bad=[]
 for offset,value in [(0,0),(4,0),(8,0),(12,0),(16,65536),(20,0),(24,1),(28,0),(24+4*(banks+1),0xffff0001),(info.meta,0),(info.meta+4,0),(info.meta+8,0)]:
  b=bytearray(raw);struct.pack_into('>I',b,offset,value);bad.append((bytes(b),len(final)))
 bad.extend([(raw[:-1],len(final)),(raw,len(final)-1)])
 for b,cap in bad:
  arena=C.create_string_buffer(b+b'\xa5'*len(final));before=arena.raw;s=Song();i=Info()
  assert not fn(arena,len(b),cap,C.byref(s),C.byref(i)) and arena.raw==before
 rows.append({'track':name,'samples':info.samples,'arena_bytes':info.arena,'loop_seek_and_guards':True,'rejected_inputs':len(bad)})
(root/'docs/TRACKLOADER_TUNE_RESULTS.json').write_text(json.dumps(rows,indent=2)+'\n');print(rows)
