"""Verify the explicitly accepted soundtrack inputs without rewriting the lock."""
import hashlib
import json
from pathlib import Path
ROOT=Path(__file__).resolve().parents[2]
def verify_assets(group,root=ROOT):
    lock=json.loads((root/'soundtrack/trackloader_assets.lock.json').read_text())
    if lock['version']!=1:raise ValueError('Unsupported soundtrack lock version')
    for row in lock[group]:
        path=root/row['path']
        try:data=path.read_bytes()
        except FileNotFoundError:raise ValueError('Missing accepted asset: '+str(path)) from None
        if len(data)!=row['bytes'] or hashlib.sha256(data).hexdigest()!=row['sha256']:
            raise ValueError('Soundtrack differs from accepted asset: '+str(path)+
                             '; regenerate the accepted input or deliberately review/update soundtrack/trackloader_assets.lock.json')
