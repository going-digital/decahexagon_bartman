#!/usr/bin/env python3
"""Check repacked ADFs with the actual runtime OFS reader, not just an inspector."""
import ctypes,json,subprocess,sys,tempfile
from pathlib import Path
root=Path(__file__).resolve().parents[1]
sys.path.insert(0,str(root/'tools/trackloader'))
from pack_ofs import pack,read_cost
from native_layout import reservations
from check_adf_layout import inspect
work=root/'scratchpad/trackloader';native=work/'native_game'
original=(Path(sys.argv[1]) if len(sys.argv)>1 else native/'native_menu.adf').read_bytes()
# Derive payloads from this image, so the test also works during a rebuild.
def contents(name):
    import struct
    h=len(name)
    for c in name.upper():h=(h*13+ord(c))&0x7ff
    def words(n):return struct.unpack_from('>128I',original,n*512)
    n=words(880)[6+h%72]
    while original[n*512+433:n*512+433+len(name)]!=name.encode():n=words(n)[124]
    n=words(n)[4];result=bytearray()
    while n:
        w=words(n);result+=original[n*512+24:n*512+24+w[3]];n=w[4]
    return bytes(result)
expected={name:contents(name) for name in ('game','courtesy','otis','focus','save_a','save_b')}
packed=pack(original,expected,reservations())
assert pack(packed,expected,reservations())==packed
with tempfile.TemporaryDirectory() as tmp:
    libpath=Path(tmp)/'ofs.dylib'
    subprocess.run(['cc','-shared','-fPIC',str(root/'trackloader/ofs.c'),'-o',str(libpath)],check=True)
    lib=ctypes.CDLL(str(libpath));callback=ctypes.CFUNCTYPE(ctypes.c_int,ctypes.c_void_p,ctypes.c_uint32,ctypes.c_void_p)
    fn=lib.track_ofs_load;fn.argtypes=[callback,ctypes.c_void_p,ctypes.c_char_p,ctypes.c_void_p,ctypes.c_uint32,ctypes.c_uint32,ctypes.c_void_p]
    reports=[]
    for data in (original,packed):
        report=read_cost(data,inspect(data,expected,reservations()));reports.append(report)
        for name,payload in expected.items():
            tracks=[]
            @callback
            def read(ctx,sector,dst):
                if not tracks or tracks[-1]!=sector//11:tracks.append(sector//11)
                ctypes.memmove(dst,bytes(data[sector*512:(sector+1)*512]),512);return 1
            dst=ctypes.create_string_buffer(len(payload));scratch=ctypes.create_string_buffer(1244)
            assert fn(read,None,name.encode(),dst,len(payload),len(payload),scratch)==1
            assert dst.raw==payload
            if name in report:assert len(tracks)==report[name]['track_reads']
    assert sum(r['track_reads'] for r in reports[1].values())<=sum(r['track_reads'] for r in reports[0].values())
print(json.dumps(dict(before=reports[0],after=reports[1]),indent=2))
print('All six files match through runtime reader; reservations, ownership, checksums, bitmap and idempotence pass.')
