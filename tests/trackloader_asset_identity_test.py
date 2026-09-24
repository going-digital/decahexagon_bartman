#!/usr/bin/env python3
"""Accepted banks/cues must not silently change during asset preparation/build."""
import json
import shutil
import sys
import tempfile
from pathlib import Path
root=Path(__file__).resolve().parents[1]
sys.path.insert(0,str(root/'tools/trackloader'))
from asset_identity import verify_assets
lock=root/'soundtrack/trackloader_assets.lock.json'
with tempfile.TemporaryDirectory() as directory:
    work=Path(directory);(work/'soundtrack').mkdir()
    shutil.copy2(lock,work/'soundtrack'/lock.name)
    data=json.loads(lock.read_text())
    for group in ('banks','disk_assets'):
        for row in data[group]:
            dest=work/row['path'];dest.parent.mkdir(parents=True,exist_ok=True)
            shutil.copy2(root/row['path'],dest)
        verify_assets(group,work)
        for row in data[group]:
            path=work/row['path'];original=path.read_bytes()
            altered=bytearray(original);altered[-1]^=1;path.write_bytes(altered)
            try:verify_assets(group,work)
            except ValueError as error:assert 'differs' in str(error)
            else:raise AssertionError('Changed asset accepted')
            path.unlink()
            try:verify_assets(group,work)
            except ValueError as error:assert 'Missing' in str(error)
            else:raise AssertionError('Missing asset accepted')
            path.write_bytes(original)
print('Accepted audio identity: all 9 assets validate; changed and missing variants rejected')
