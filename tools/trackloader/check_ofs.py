#!/usr/bin/env python3
"""Exercise the bounded C reader with the real ADF and malformed OFS blocks."""
import ctypes, json, struct, subprocess
from pathlib import Path
from check_adf_layout import inspect
root=Path(__file__).resolve().parents[2];out=root/'scratchpad/trackloader';native=out/'native_game'
libpath=out/'ofs.dylib'
subprocess.run(['cc','-std=c99','-Wall','-Wextra','-Werror','-shared','-fPIC',str(root/'trackloader/ofs.c'),'-o',str(libpath)],check=True)
lib=ctypes.CDLL(str(libpath));read_type=ctypes.CFUNCTYPE(ctypes.c_int,ctypes.c_void_p,ctypes.c_uint32,ctypes.c_void_p)
fn=lib.track_ofs_load;fn.argtypes=[read_type,ctypes.c_void_p,ctypes.c_char_p,ctypes.c_void_p,ctypes.c_uint32,ctypes.c_uint32,ctypes.c_void_p];fn.restype=ctypes.c_int
original=(native/'native_menu.adf').read_bytes()
expected={t:(out/f'{t}.fibonacci.deflate').read_bytes() for t in ['courtesy','focus','otis']}
expected.update(game=(native/'game.deflate').read_bytes(),save_a=bytes([51])*512,save_b=bytes([34])*512)
reservations=json.loads((root/'docs/TRACKLOADER_RESERVED_LAYOUT.json').read_text())['reservations']
layout=inspect(original,expected,reservations)
def run(data,name='courtesy',success=False,fail_sector=None,capacity=None,untouched=False):
    payload=expected.get(name.lower(),expected['courtesy']);n=len(payload)
    capacity=n if capacity is None else capacity
    dst=ctypes.create_string_buffer(b'\xa5'*(n+32));scratch=ctypes.create_string_buffer(b'\xa5'*(1244+32));calls=[];errors=[]
    @read_type
    def read(ctx,sector,buffer):
        if not (2<=sector<1661 and sector not in calls):
            errors.append(sector);return 0
        calls.append(sector)
        if sector==fail_sector:return 0
        ctypes.memmove(buffer,bytes(data[sector*512:(sector+1)*512]),512);return 1
    result=fn(read,None,name.encode(),ctypes.addressof(dst)+16,capacity,n,ctypes.addressof(scratch)+16)
    assert not errors,errors
    assert bool(result)==success,(name,result,calls[-5:])
    assert dst.raw[:16]==b'\xa5'*16 and dst.raw[16+n:16+n+16]==b'\xa5'*16
    assert scratch.raw[:16]==b'\xa5'*16 and scratch.raw[1260:1276]==b'\xa5'*16
    if success:assert dst.raw[16:16+n]==payload
    if untouched:assert dst.raw[16:16+n]==b'\xa5'*n
    assert len(calls)<=1659
    return len(calls)
valid={name:run(original,name,True) for name in expected}
run(original,'COURTESY',True)
file=layout['files']['courtesy'];header=file['header'];first=file['data_sectors'][0];last=file['data_sectors'][-1];ext=file['metadata_sectors'][1]
def change(sector,index,value,checksum=True):
    data=bytearray(original);struct.pack_into('>I',data,sector*512+index*4,value)
    if checksum:
        offset=5;struct.pack_into('>I',data,sector*512+offset*4,0)
        total=sum(struct.unpack_from('>128I',data,sector*512))
        struct.pack_into('>I',data,sector*512+offset*4,(-total)&0xffffffff)
    return data
cases=[('oversize',header,81,len(expected['courtesy'])+1),('undersize',header,81,1),
 ('block_length',first,3,489),('zero_length',first,3,0),('data_cycle',first,4,first),
 ('wrong_owner',first,1,880),('wrong_sequence',first,2,2),('wrong_data_type',first,0,2),
 ('out_of_disk',header,77,1760),('reserved_track',header,77,1661),
 ('extension_cycle',ext,126,ext),('bad_extension_type',ext,0,2),
 ('too_many_pointers',header,2,73),('zero_pointers',header,2,0),
 ('trailing_chain',last,4,first),('root_type',880,0,0)]
for label,sector,index,value in cases:run(change(sector,index,value),untouched=label in ['oversize','undersize'])
run(change(first,3,1,False));run(original,fail_sector=first,untouched=True)
run(original,capacity=len(expected['courtesy'])-1,untouched=True)
run(original,'missing');run(original,'DF0:courtesy');run(original,'x'*31)
# Self-loop in the hash chain of a nonmatching filename.
data=change(header,124,header);data[header*512+433]^=1
struct.pack_into('>I',data,header*512+20,0)
struct.pack_into('>I',data,header*512+20,(-sum(struct.unpack_from('>128I',data,header*512)))&0xffffffff)
run(data)
report=dict(status='Bounded host C OFS reader matches real disk payloads and rejects malformed controls',valid_sector_reads=valid,malformed_cases=[x[0] for x in cases]+['checksum','read_failure','capacity','missing','path','long_name','hash_cycle'],limitations=['This report covers host C execution; native integration is recorded separately','Failure may leave validated prefix in destination; caller must not publish it'])
(root/'docs/TRACKLOADER_OFS_RESULTS.json').write_text(json.dumps(report,indent=2)+'\n');print(json.dumps(report,indent=2))
