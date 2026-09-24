#!/usr/bin/env python3
"""Build both Amiga editions and atomically publish one reproducible ZIP."""
import argparse
import hashlib
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import zipfile

ROOT = Path(__file__).resolve().parents[1]


def sha(data):
    return hashlib.sha256(data).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--execram-source', type=Path, required=True)
    parser.add_argument('--whdload-sdk', type=Path,
                        default=ROOT/'scratchpad/whdload/sdk/WHDLoad')
    args = parser.parse_args()
    if not (args.whdload_sdk/'Include/whdload.i').is_file():
        parser.error('Missing WHDLoad SDK: '+str(args.whdload_sdk))
    subprocess.run([sys.executable, str(ROOT/'tools/trackloader/build_release.py'),
                    '--execram-source', str(args.execram_source.resolve())], cwd=ROOT, check=True)
    subprocess.run([sys.executable, str(ROOT/'tools/whdload/build.py'),
                    '--sdk', str(args.whdload_sdk.resolve()), '--no-archive'], cwd=ROOT, check=True)

    boot = json.loads((ROOT/'docs/TRACKLOADER_NATIVE_BOOT_RESULTS.json').read_text())
    digest = boot['adf_sha256']
    adf = (ROOT/'out/trackloader'/f'{digest}.adf').read_bytes()
    assert len(adf) == 901120 and sha(adf) == digest
    assert all(adf[n*512:(n+1)*512] == bytes(512) for n in (1738,1749))
    files = {'hexagon/ADF/hexagon.adf': adf}
    files['hexagon/ADF/manifest.json'] = (ROOT/'out/trackloader'/f'{digest}.json').read_bytes()
    whd = ROOT/'out/whdload/Hexagon'
    manifest = json.loads((whd/'manifest.json').read_text())
    for name, info in manifest['files'].items():
        path = Path(name)
        assert not path.is_absolute() and '..' not in path.parts
        data = (whd/path).read_bytes()
        assert len(data) == info['bytes'] and sha(data) == info['sha256'], name
        files['hexagon/WHDLoad/Hexagon/'+name] = data
    files['hexagon/WHDLoad/Hexagon/manifest.json'] = (whd/'manifest.json').read_bytes()
    files['hexagon/README.txt'] = (ROOT/'docs/DISTRIBUTION_README.txt').read_bytes()
    index = {'format': 1, 'files': {name: {'bytes': len(data), 'sha256': sha(data)}
                                  for name, data in sorted(files.items())}}
    files['hexagon/manifest.json'] = (json.dumps(index, indent=2)+'\n').encode()
    # Check names and uncompressed content, including slave strings and icons.
    for name, data in files.items():
        assert 'decahexagon' not in name.lower(), name
        assert b'decahexagon' not in data.lower(), name
    output = ROOT/'out/Hexagon.zip'
    # Keep the previous distribution intact until both builds and ZIP verification pass.
    with tempfile.TemporaryDirectory(dir=output.parent) as temporary:
        pending = Path(temporary)/output.name
        with zipfile.ZipFile(pending, 'w') as archive:
            for name, data in sorted(files.items()):
                entry = zipfile.ZipInfo(name, (2026,1,1,0,0,0))
                entry.compress_type = zipfile.ZIP_DEFLATED
                entry.external_attr = 0o100644 << 16
                archive.writestr(entry, data)
        with zipfile.ZipFile(pending) as archive:
            assert archive.testzip() is None
            assert set(archive.namelist()) == set(files)
            assert all(archive.read(name) == data for name, data in files.items())
        pending.replace(output)
    print('Distribution ZIP:', output)


if __name__ == '__main__':
    main()
