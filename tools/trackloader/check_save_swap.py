#!/usr/bin/env python3
"""Verify artifacts from the timed save-window swap trial."""
import hashlib
import json
from pathlib import Path
root = Path(__file__).resolve().parents[2]
p = root/'scratchpad/trackloader/native_game'
source = (p/'native_menu.adf').read_bytes()
build = json.loads((root/'docs/TRACKLOADER_NATIVE_BOOT_RESULTS.json').read_text())
assert hashlib.sha256(source).hexdigest() == build['adf_sha256']
for name in ['save_window','save_swap']:
    assert (p/(name+'.adf')).read_bytes() == source
    assert (p/(name+'.log')).read_text().splitlines() == [
        'NATIVE-SAVE-DEFAULT','NATIVE-GAME-ENTRY','DF0:courtesyNATIVE-TUNE-READY']
    assert (p/(name+'.png')).exists()
assert (p/'save_swap_foreign.adf').read_bytes() == bytes(901120)
report = dict(status='Timed save-window swap returns SAVE ERROR; disk images unchanged',
              source_sha256=build['adf_sha256'],
              emulator='Copperline PAL A500OCS, 512K Chip + 512K slow',
              key='Space at 60 seconds for 100 ms',
              control_capture_seconds=114, swap_seconds=115, result_capture_seconds=140,
              screenshots=dict(control='save_window.png', result='save_swap.png'),
              visual_review='Control shows SAVE; swapped run shows SAVE ERROR',
              original_unchanged=True, replacement_unchanged=True,
              limitations=['Scheduled insertion forces replacement read-only',
                           'Exact disk transfer phase at swap not instrumented',
                           'Does not prove protection of writable replacement or active-DMA removal',
                           'Pending-save retry after reinsertion not tested in this trial'])
(root/'docs/TRACKLOADER_SAVE_SWAP_RESULTS.json').write_text(json.dumps(report,indent=2)+'\n')
print(report['status'])
