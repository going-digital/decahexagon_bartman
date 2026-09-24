#!/usr/bin/env python3
"""Run the real save transaction against a disposable copy of the native ADF.

Sector callbacks model image I/O, not DiskIO DMA or physical track writes.
"""
import ctypes,hashlib,json,subprocess
from pathlib import Path
root=Path(__file__).resolve().parents[2];native=root/'scratchpad/trackloader/native_game'
manifest=json.loads((native/'save_manifest.json').read_text())
original=(native/'native_menu.adf').read_bytes()
assert hashlib.sha256(original).hexdigest()==manifest['image_sha256']
libpath=native/'save_disk.dylib'
subprocess.run(['cc','-std=c99','-Wall','-Wextra','-Werror','-shared','-fPIC',str(root/'trackloader/save.c'),str(root/'trackloader/save_disk.c'),'-o',str(libpath)],check=True)
class Records(ctypes.Structure):_fields_=[('best',ctypes.c_uint32*6),('completed',ctypes.c_uint8*6)]
class Save(ctypes.Structure):_fields_=[('records',Records),('achievements',ctypes.c_uint32),('generation',ctypes.c_uint32)]
class Media(ctypes.Structure):_fields_=[('identity',ctypes.c_ubyte*16),('sector',ctypes.c_uint32*3),('expected',ctypes.c_void_p*3)]
callback=ctypes.CFUNCTYPE(ctypes.c_int,ctypes.c_void_p,ctypes.c_uint32,ctypes.c_void_p)
lib=ctypes.CDLL(str(libpath));load=lib.track_save_disk_load;commit=lib.track_save_disk_commit
load.argtypes=[callback,ctypes.c_void_p,ctypes.POINTER(Media),ctypes.POINTER(Save),ctypes.c_void_p]
commit.argtypes=[callback,callback,ctypes.c_void_p,ctypes.POINTER(Media),ctypes.POINTER(Save),ctypes.c_void_p]
anchors=[ctypes.create_string_buffer(bytes.fromhex(a['hex'])) for a in manifest['anchors']]
media=Media((ctypes.c_ubyte*16).from_buffer_copy(bytes.fromhex(manifest['identity'])),(ctypes.c_uint32*3)(*[a['sector'] for a in manifest['anchors']]),(ctypes.c_void_p*3)(*[ctypes.addressof(a) for a in anchors]))
disk=bytearray(original);writes=[];errors=[]
@callback
def read(ctx,sector,buffer):
 if sector>=1760:errors.append(sector);return 0
 ctypes.memmove(buffer,bytes(disk[sector*512:(sector+1)*512]),512);return 1
@callback
def write(ctx,sector,buffer):
 if sector not in (1738,1749):errors.append(sector);return 0
 disk[sector*512:(sector+1)*512]=ctypes.string_at(buffer,512);writes.append(sector);return 1
def scratch():return ctypes.create_string_buffer(2048)
state=Save();assert load(read,None,ctypes.byref(media),ctypes.byref(state),scratch())==2
state.generation=1;state.records.best[0]=4000;state.records.completed[0]=1
assert commit(read,write,None,ctypes.byref(media),ctypes.byref(state),scratch())==1
state.generation=2;state.records.best[1]=5000;state.records.completed[1]=1
assert commit(read,write,None,ctypes.byref(media),ctypes.byref(state),scratch())==1
path=native/'save_recovery_trial.adf';path.write_bytes(disk)
# Discard RAM state and reopen the image: storage-level cold recovery.
disk=bytearray(path.read_bytes());recovered=Save()
assert load(read,None,ctypes.byref(media),ctypes.byref(recovered),scratch())==1
assert recovered.generation==2 and list(recovered.records.best)[:2]==[4000,5000]
assert commit(read,write,None,ctypes.byref(media),ctypes.byref(recovered),scratch())==1
assert writes==[1738,1749] # retrying committed state did not write
# A damaged newest slot must leave the older committed records recoverable.
disk[1749*512+100]^=1
assert load(read,None,ctypes.byref(media),ctypes.byref(recovered),scratch())==1
assert recovered.generation==1 and list(recovered.records.best)[:2]==[4000,0]
for sector in range(1760):
 if sector not in (1738,1749):assert disk[sector*512:(sector+1)*512]==original[sector*512:(sector+1)*512]
assert not errors
assert (native/'native_menu.adf').read_bytes()==original
report=dict(status='Actual C save transaction on disposable ADF: reopen, latest state, idempotent retry and damaged-newest fallback pass',source_sha256=manifest['image_sha256'],writes=writes,artifact=str(path.relative_to(root)),outside_slots_unchanged=True,limitations=['Host sector callbacks; no emulated boot or native DiskIO writes','Physical torn-track behavior is not modeled'])
(root/'docs/TRACKLOADER_SAVE_IMAGE_RESULTS.json').write_text(json.dumps(report,indent=2)+'\n');print(report)
