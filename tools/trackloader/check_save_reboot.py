#!/usr/bin/env python3
"""Save and cold-boot a disposable copy of the current production disk."""
import argparse
import hashlib
import json
import struct
import subprocess
import time
import zlib
from pathlib import Path

p = argparse.ArgumentParser(description=__doc__)
p.add_argument('--rom', type=Path, required=True)
p.add_argument('--video', choices=['PAL', 'NTSC'], required=True)
a = p.parse_args()
root = Path(__file__).resolve().parents[2]
base = root/'scratchpad/trackloader/native_game'
out = base/f'save_reboot_{a.video.lower()}_{time.time_ns()}'
out.mkdir()
original = (base/'native_menu.adf').read_bytes()
digest = hashlib.sha256(original).hexdigest()
assert digest == json.loads((root/'docs/TRACKLOADER_NATIVE_BOOT_RESULTS.json').read_text())['adf_sha256']
identity = (base/'save_identity.bin').read_bytes()
assert len(original) == 901120
assert all(original[s*512:(s+1)*512] == bytes(512) for s in (1738, 1749))
disk = out/'saved.adf'
disk.write_bytes(original)

def trial(name, readonly, seconds, keys):
    config = out/(name+'.toml')
    config.write_text('[emulation]\npacing_budget = "cycles"\n[floppy.df0]\npath = '+json.dumps(str(disk))+'\nwrite_protected = '+str(readonly).lower()+'\n')
    command = ['copperline', '--config', str(config), '--model', 'A500OCS',
               '--chip', '512K', '--slow', '512K', '--video', a.video,
               '--noaudio', '--serial', 'stdout']
    for at, key in keys:
        command += ['--key-after', str(at), hex(key), '100']
    command += ['--screenshot-after', str(seconds), str(out/(name+'.png')), str(a.rom)]
    with (out/(name+'.log')).open('w') as log:
        subprocess.run(command, stdout=log, stderr=subprocess.STDOUT, check=True, timeout=600)
    assert (out/(name+'.png')).is_file()
    return (out/(name+'.log')).read_text().splitlines()

assert trial('save', False, 180, [(60, 0x40)]) == [
    'NATIVE-SAVE-DEFAULT', 'NATIVE-GAME-ENTRY', 'DF0:courtesyNATIVE-TUNE-READY']
saved = disk.read_bytes()
assert len(saved) == len(original)
changed = [s for s in range(1760) if saved[s*512:(s+1)*512] != original[s*512:(s+1)*512]]
assert changed == [1738], changed
slot = saved[1738*512:1739*512]
assert struct.unpack_from('>4I', slot) == (0x48585331, 1, 512, 1)
assert slot[16:32] == identity
assert struct.unpack_from('>I', slot, 508)[0] == zlib.crc32(slot[:508])
best = list(struct.unpack_from('>6I', slot, 32))
assert best[0] > 0 and not any(best[1:])
assert not any(slot[64:508])
assert trial('reboot', True, 50, []) == ['NATIVE-SAVE-RESTORED', 'NATIVE-GAME-ENTRY']
assert disk.read_bytes() == saved, 'Read-only cold reboot changed saved disk'
assert (base/'native_menu.adf').read_bytes() == original
report = dict(status=a.video+' current-image save and cold reboot pass',
              source_sha256=digest, saved_sha256=hashlib.sha256(saved).hexdigest(),
              identity=identity.hex(), changed_sectors=changed, generation=1,
              best_ticks=best, reboot_disk_unchanged=True, factory_disk_unchanged=True,
              evidence=str(out.relative_to(root)),
              limitations=['Basic record persistence; no completion or achievement trigger tested',
                           'No interrupted-write or physical hardware test',
                           'Screenshot content requires separate visual review'])
(root/f'docs/TRACKLOADER_SAVE_REBOOT_{a.video}_RESULTS.json').write_text(json.dumps(report, indent=2)+'\n')
print(json.dumps(report, indent=2))
