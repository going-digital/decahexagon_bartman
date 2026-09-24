#!/usr/bin/env python3
"""Independent endian/CRC and valid-checksum malformed save checks."""
import ctypes,json,struct,subprocess,zlib
from pathlib import Path
root=Path(__file__).resolve().parents[2];out=root/'scratchpad/trackloader'
libpath=out/'save.dylib'
subprocess.run(['cc','-std=c99','-Wall','-Wextra','-Werror','-shared','-fPIC',str(root/'trackloader/save.c'),'-o',str(libpath)],check=True)
class Records(ctypes.Structure):_fields_=[('best',ctypes.c_uint32*6),('completed',ctypes.c_uint8*6)]
class Save(ctypes.Structure):_fields_=[('records',Records),('achievements',ctypes.c_uint32),('generation',ctypes.c_uint32)]
lib=ctypes.CDLL(str(libpath));enc=lib.track_save_encode;dec=lib.track_save_decode
enc.argtypes=[ctypes.c_void_p,ctypes.c_void_p,ctypes.POINTER(Save)];dec.argtypes=enc.argtypes
state=Save(Records((ctypes.c_uint32*6)(1,3600,3601,0xffffffff,5,6),(ctypes.c_uint8*6)(0,0,1,1,0,1)),0x87654321,0xfffffffe)
id=b'0123456789abcdef';slot=ctypes.create_string_buffer(512)
assert enc(slot,id,ctypes.byref(state))
expected=bytearray(512);struct.pack_into('>4I',expected,0,0x48585331,1,512,0xfffffffe);expected[16:32]=id
struct.pack_into('>8I',expected,32,1,3600,3601,0xffffffff,5,6,44,0x87654321)
struct.pack_into('>I',expected,508,zlib.crc32(expected[:508]))
assert slot.raw==expected
rejected=[]
for name,offset,value in [('magic',0,0),('old_version',4,0),('future_version',4,2),('size',8,64),('completion_bits',56,64),('reserved',64,1)]:
    changed=bytearray(expected);struct.pack_into('>I',changed,offset,value);struct.pack_into('>I',changed,508,zlib.crc32(changed[:508]))
    result=Save();ctypes.memset(ctypes.byref(result),0xa5,ctypes.sizeof(result));before=bytes(result)
    assert not dec(bytes(changed),id,ctypes.byref(result)) and bytes(result)==before
    rejected.append(name)
report=dict(status='Exact endian layout and CRC match independent Python encoder; valid-CRC malformed records rejected',slot_bytes=512,crc32=f'{zlib.crc32(expected[:508]):08x}',rejected=rejected,limitations=['This report covers the RAM codec only; native tests are reported separately'])
(root/'docs/TRACKLOADER_SAVE_FORMAT_RESULTS.json').write_text(json.dumps(report,indent=2)+'\n');print(report)
