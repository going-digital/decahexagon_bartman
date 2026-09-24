#!/usr/bin/env python3
"""Independently walk the generated flat OFS volume; verify ownership and content."""
import hashlib,json,struct
from pathlib import Path
ROOT=Path(__file__).resolve().parents[2]
WORK=ROOT/'scratchpad/trackloader'

def inspect(data,expected,reservations=None):
 assert len(data)==1760*512
 def block(n):
  assert 2<=n<1760,('sector outside filesystem',n)
  w=struct.unpack_from('>128I',data,n*512)
  assert sum(w)&0xffffffff==0,('checksum',n)
  return w
 root=block(880);assert root[0]==2 and root[3]==72 and root[127]==1
 assert root[78]==0xffffffff and root[104]==0 # single bitmap, no extension
 bitmap_sector=root[79];assert all(x==0 for x in root[80:104])
 bitmap=block(bitmap_sector)
 owners={0:'boot',1:'boot',880:'root',bitmap_sector:'bitmap'}
 def own(n,owner):
  assert n not in owners,('duplicate owner',n,owner,owners.get(n))
  owners[n]=owner
 for name,sectors in (reservations or {}).items():
  for n in sectors:own(n,'reserved:'+name)
 files={}
 for bucket,first in enumerate(root[6:78]):
  current=first
  while current:
   header=block(current);assert header[0]==2 and header[127]==0xfffffffd and header[1]==current and header[125]==880
   raw=data[current*512:(current+1)*512];length=raw[432];assert 0<length<=30
   name=raw[433:433+length].decode('ascii');assert name not in files
   h=len(name)
   for c in name.upper():h=(h*13+ord(c))&0x7ff
   assert h%72==bucket,('hash bucket',name)
   own(current,name+':header');meta=[current];sectors=[];w=header
   while True:
    assert w[2]<=72
    sectors.extend(reversed(w[78-w[2]:78]))
    ext=w[126]
    if not ext:break
    own(ext,name+':extension');meta.append(ext);w=block(ext)
    assert w[0]==16 and w[127]==0xfffffffd and w[125]==880 and w[1]==ext
   content=bytearray()
   assert header[4]==(sectors[0] if sectors else 0)
   for i,n in enumerate(sectors):
    own(n,name+':data');d=block(n)
    assert d[0]==8 and d[1]==meta[i//72] and d[2]==i+1 and 0<d[3]<=488
    assert d[4]==(sectors[i+1] if i+1<len(sectors) else 0)
    content+=data[n*512+24:n*512+24+d[3]]
   assert len(content)==header[81]
   assert name in expected and content==expected[name],('contents',name)
   files[name]={'bytes':len(content),'header':current,'metadata_sectors':meta,'data_sectors':sectors,'physical_tracks':sorted({s//11 for s in meta+sectors}),'sha256':hashlib.sha256(content).hexdigest()}
   current=header[124]
 assert set(files)==set(expected)
 free=[]
 for n in range(2,1760):
  available=bool(bitmap[1+(n-2)//32]&(1<<((n-2)%32)))
  assert available==(n not in owners),('bitmap/ownership mismatch',n)
  if available:free.append(n)
 tracks=[t for t in range(160) if all(s in free for s in range(t*11,t*11+11))]
 return {'files':files,'free_sectors':len(free),'wholly_free_tracks':tracks,'save_tracks_disjoint':set(files['save_a']['physical_tracks']).isdisjoint(files['save_b']['physical_tracks'])}

if __name__=='__main__':
 expected={t:(WORK/f'{t}.fibonacci.deflate').read_bytes() for t in ['courtesy','focus','otis']}
 expected.update(game=(WORK/'game_budget/out/game_packed.exe').read_bytes(),save_a=bytes([51])*512,save_b=bytes([34])*512)
 data=(WORK/'filesystem_trial.adf').read_bytes();result=inspect(data,expected)
 # Independent negative controls: corruption must not silently pass.
 for sector in [880,result['files']['focus']['data_sectors'][0]]:
  broken=bytearray(data);broken[sector*512+24]^=1
  try:inspect(broken,expected)
  except AssertionError:pass
  else:raise AssertionError('corruption accepted')
 result['status']='Independent OFS traversal: all file bytes, checksums, chains, hash buckets and bitmap ownership verified; two corruption controls rejected'
 (ROOT/'docs/TRACKLOADER_DISK_LAYOUT.json').write_text(json.dumps(result,indent=2)+'\n')
 print(json.dumps({k:v for k,v in result.items() if k!='files'},indent=2))
