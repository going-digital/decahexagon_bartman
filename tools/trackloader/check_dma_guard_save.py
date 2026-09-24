#!/usr/bin/env python3
"""Verify the latest DMA-guard save and read-only cold-reboot artifacts."""
import hashlib
import json
import struct
import zlib
from pathlib import Path
root = Path(__file__).resolve().parents[2]
p = root/'scratchpad/trackloader/native_game'
original = (p/'native_menu.adf').read_bytes()
saved = (p/'dma_guard_save.adf').read_bytes()
reboot = (p/'dma_guard_reboot.adf').read_bytes()
build = json.loads((root/'docs/TRACKLOADER_NATIVE_BOOT_RESULTS.json').read_text())
assert hashlib.sha256(original).hexdigest() == build['adf_sha256']
assert saved == reboot, 'Read-only reboot changed the disk'
changed = [i for i in range(1760) if original[i*512:(i+1)*512] != saved[i*512:(i+1)*512]]
assert changed == [1738], changed
slot = saved[1738*512:1739*512]
assert struct.unpack_from('>4I', slot) == (0x48585331, 1, 512, 1)
assert slot[16:32] == (p/'save_identity.bin').read_bytes()
assert struct.unpack_from('>I', slot, 508)[0] == zlib.crc32(slot[:508])
assert not any(slot[64:508])
best = struct.unpack_from('>6I', slot, 32)
assert best[0] > 0 and not any(best[1:])
for name, expected in {
    'dma_guard_save': ['NATIVE-SAVE-DEFAULT','NATIVE-GAME-ENTRY','DF0:courtesyNATIVE-TUNE-READY'],
    'dma_guard_reboot': ['NATIVE-SAVE-RESTORED','NATIVE-GAME-ENTRY'],
}.items():
    assert (p/(name+'.log')).read_text().splitlines() == expected
    assert (p/(name+'.png')).exists()
report = dict(status='Latest DMA-guard image boots, saves and restores on cold reboot',
              source_sha256=build['adf_sha256'], saved_sha256=hashlib.sha256(saved).hexdigest(),
              emulator='Copperline PAL A500OCS, 512K Chip + 512K slow',
              changed_sectors=changed, generation=1, best_ticks=best[0],
              stage_bytes=build['stage_bytes'], reboot_disk_unchanged=True,
              screenshots=['dma_guard_save.png','dma_guard_reboot.png'],
              limitations=['No timed disk removal or physical-drive test',
                           'Screenshot content reviewed separately; artifact association relies on trial procedure'])
(root/'docs/TRACKLOADER_DMA_GUARD_SAVE_RESULTS.json').write_text(json.dumps(report,indent=2)+'\n')
print(json.dumps(report,indent=2))
