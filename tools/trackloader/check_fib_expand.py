#!/usr/bin/env python3
"""Exercise reverse-block expansion with the actual C decoder and guarded arenas."""
import ctypes as C,hashlib,json,random,struct,subprocess,sys,zlib
from pathlib import Path
root=Path(__file__).resolve().parents[2];out=root/'scratchpad/trackloader'
sys.path.insert(0,str(root/'tools/audio'))
from fib_reference import fib_decode
subprocess.run(['cc','-O2','-Wall','-Wextra','-Werror','-shared','-fPIC',str(root/'trackloader/fib_expand.c'),str(root/'fib_decode.c'),'-o',str(out/'fib_expand.dylib')],check=True)
lib=C.CDLL(str(out/'fib_expand.dylib'));fn=lib.trackloader_fib_expand
fn.argtypes=[C.c_void_p,C.c_uint,C.c_uint];fn.restype=C.c_uint
rows=[]
def check(name,blob):
 expected=fib_decode(blob);cap=max(len(blob),len(expected));buf=C.create_string_buffer(cap+32)
 C.memset(buf,0xa5,len(buf));C.memmove(C.byref(buf,16),blob,len(blob))
 assert fn(C.byref(buf,16),len(blob),cap)==len(expected)
 assert buf.raw[16:16+len(expected)]==expected
 assert buf.raw[:16]==b'\xa5'*16 and buf.raw[-16:]==b'\xa5'*16
 return dict(name=name,input_bytes=len(blob),output_bytes=len(expected),arena_bytes=cap,scratch_bytes=257,sha256=hashlib.sha256(expected).hexdigest())
for track in ['courtesy','focus','otis']:
 package=zlib.decompress((out/f'{track}.fibonacci.deflate').read_bytes(),-15)
 magic,rate,samples,entries,seqcount,pcmbytes=struct.unpack_from('>4s5I',package)
 headerlen=24+4*(entries+1)+4*seqcount
 row=check(track,package[headerlen:]);row['metadata_bytes']=headerlen
 row['combined_arena_bytes']=headerlen+max(row['input_bytes'],row['output_bytes'])
 assert row['output_bytes']==pcmbytes
 rows.append(row)
# Arbitrary codes cover wrapping, odd tails and block boundaries without relying
# on an encoder's choices. Independent Python decoder supplies expected output.
rng=random.Random(31)
for n in list(range(1,1026))+[1535,1536,1537,4095,4096,4097]:
 blob=struct.pack('<4sII',b'FIB1',n,512)
 for pos in range(0,n,512):blob+=rng.randbytes(1+min(512,n-pos)//2)
 check(str(n),blob)
# Rejected layouts must leave the entire allocation untouched.
valid=struct.pack('<4sII',b'FIB1',512,512)+bytes(257)
for blob,cap in [(valid[:-1],512),(valid+b'x',512),(b'BAD!'+valid[4:],512),(valid,511),(struct.pack('<4sII',b'FIB1',0,512),512),(struct.pack('<4sII',b'FIB1',512,256)+bytes(257),512)]:
 buf=C.create_string_buffer(blob+b'\xa5'*600);before=buf.raw
 assert fn(buf,len(blob),cap)==0 and buf.raw==before
report={'status':'Host execution of actual C decoder; target timing and combined inflate/expand execution remain untested','trials':rows,'synthetic_lengths':1031,'malformed_cases':6}
(root/'docs/TRACKLOADER_FIB_EXPANSION_RESULTS.json').write_text(json.dumps(report,indent=2)+'\n')
print(json.dumps(report,indent=2))
