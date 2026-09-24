#!/usr/bin/env python3
"""Generate trusted save-media anchors from the verified build, never at runtime."""
import hashlib,json,struct
from pathlib import Path
from native_layout import reservations as native_reservations
from check_adf_layout import inspect
root=Path(__file__).resolve().parents[2];out=root/'scratchpad/trackloader';native=out/'native_game'
data=(native/'native_menu.adf').read_bytes()
sha=hashlib.sha256(data).hexdigest()
assert sha==json.loads((root/'docs/TRACKLOADER_NATIVE_BOOT_RESULTS.json').read_text())['adf_sha256']
reservations=native_reservations()
expected={t:(out/f'{t}.fibonacci.deflate').read_bytes() for t in ['courtesy','focus','otis']}
expected.update(game=(native/'game.deflate').read_bytes(),save_a=bytes([51])*512,save_b=bytes([34])*512)
inspect(data,expected,reservations)
assert reservations['persistent_a']==list(range(1738,1749))
assert reservations['persistent_b']==list(range(1749,1760))
bitmap=struct.unpack_from('>I',data,880*512+316)[0]
sectors=[0,880,bitmap];assert len(set(sectors))==3
anchors=[data[s*512:(s+1)*512] for s in sectors]
for sector in (1738,1749):assert data[sector*512:(sector+1)*512]==bytes(512),'factory slots must be blank'
identity=hashlib.sha256(b'HXS1 payload identity\0'+b''.join(hashlib.sha256(expected[n]).digest() for n in ['game','courtesy','otis','focus'])).digest()[:16]
manifest=dict(format='HXS1',identity=identity.hex(),image_sha256=sha,slots=[1738,1749],anchors=[dict(sector=s,hex=b.hex()) for s,b in zip(sectors,anchors)],limitations=['Identity and compressed write-authorization anchors embedded in native stage','Runtime adapter must also detect media changes and enforce track ownership','Build-specific identity; migration between builds is not implemented'])
(native/'save_manifest.json').write_text(json.dumps(manifest,indent=2)+'\n')
print('Save manifest: verified immutable anchors and separate reserved slot tracks; identity '+identity.hex())
