#!/usr/bin/env python3
"""Verify artifacts from the dual-standard native boot smoke tests."""
import hashlib,json
from pathlib import Path
root=Path(__file__).resolve().parents[2];p=root/'scratchpad/trackloader/native_game'
image=(p/'native_menu.adf').read_bytes();trials=[]
for standard in ('pal','ntsc'):
    name='auto_'+standard
    assert (p/(name+'.adf')).read_bytes()==image
    assert (p/(name+'.log')).read_text().splitlines()==[
        'NATIVE-SAVE-DEFAULT','NATIVE-GAME-ENTRY','DF0:courtesyNATIVE-TUNE-READY']
    assert (p/(name+'.png')).exists()
    trials.append(dict(standard=standard,key_seconds=60,capture_seconds=140,
                       disk_unchanged=True,screenshot=name+'.png'))
report=dict(status='PAL and NTSC native boot/Courtesy-load smoke tests pass',
            source_sha256=hashlib.sha256(image).hexdigest(),trials=trials,
            visual_review='Both show rendered SAVE ERROR on write-protected disks; NTSC uses shorter display',
            limitations=['Silent runs: audio timing/quality not measured','NTSC multi-track switching and writable save lifecycle pending','No physical hardware test'])
(root/'docs/TRACKLOADER_VIDEO_BOOT_RESULTS.json').write_text(json.dumps(report,indent=2)+'\n')
print(report['status'])
