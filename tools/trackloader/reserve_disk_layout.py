#!/usr/bin/env python3
"""Reserve raw bootstrap and separate save tracks in a copy of the trial ADF."""
import hashlib,json,struct
from check_adf_layout import ROOT,WORK,inspect
expected={t:(WORK/f'{t}.fibonacci.deflate').read_bytes() for t in ['courtesy','focus','otis']}
expected.update(game=(WORK/'game_budget/out/game_packed.exe').read_bytes(),save_a=bytes([51])*512,save_b=bytes([34])*512)
data=bytearray((WORK/'filesystem_trial.adf').read_bytes());before=inspect(data,expected)
tracks=before['wholly_free_tracks'];assert len(tracks)>=6
reservations={'bootstrap':[s for t in tracks[:4] for s in range(t*11,t*11+11)],'persistent_a':list(range(tracks[-2]*11,(tracks[-2]+1)*11)),'persistent_b':list(range(tracks[-1]*11,(tracks[-1]+1)*11))}
bitmap_sector=struct.unpack_from('>I',data,880*512+316)[0]
offset=bitmap_sector*512;words=list(struct.unpack_from('>128I',data,offset))
for sectors in reservations.values():
 for sector in sectors:words[1+(sector-2)//32]&=~(1<<((sector-2)%32))
words[0]=0;words[0]=(-sum(words))&0xffffffff
struct.pack_into('>128I',data,offset,*words)
after=inspect(data,expected,reservations)
assert set(s//11 for s in reservations['persistent_a']).isdisjoint(s//11 for s in reservations['persistent_b'])
# No file or directory sector changed; only the allocation bitmap changes.
original=(WORK/'filesystem_trial.adf').read_bytes()
assert data[:offset]==original[:offset] and data[offset+512:]==original[offset+512:]
(WORK/'reserved_layout.adf').write_bytes(data)
report={'status':'Provisional nonbootable image; raw regions protected in bitmap; existing files preserved; no save journal or DMA test yet','reservations':reservations,'free_sectors':after['free_sectors'],'free_bytes':after['free_sectors']*512,'image_sha256':hashlib.sha256(data).hexdigest()}
(ROOT/'docs/TRACKLOADER_RESERVED_LAYOUT.json').write_text(json.dumps(report,indent=2)+'\n')
print('Reserved bootstrap:',len(reservations['bootstrap'])*512,'bytes; save tracks:',tracks[-2:],'remaining:',report['free_bytes'],'bytes')
