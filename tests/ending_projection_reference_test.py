"""Check the recovered stage-4 transform equations against native captures.
Host reference only: this is not the Amiga fixed-point renderer.
"""
import math
from pathlib import Path
rows = Path('tests/fixtures/pc_ending_projection_native.txt').read_text().splitlines()
for row in rows:
    t, o, d, x, y, z, nx, ny, nz, sx, sy = map(float, row.split())
    # PC truncates wrapped degree angles before indexing its trig tables.
    a = math.radians(int((360-t) % 360))
    y, z = y*math.cos(a)-z*math.sin(a), y*math.sin(a)+z*math.cos(a)
    a = math.radians(int((360+o) % 360))
    x, z = x*math.cos(a)+z*math.sin(a), z*math.cos(a)-x*math.sin(a)
    z += d-5*t
    z = abs(z) if z else 0.1
    assert math.isclose(x,nx,abs_tol=1e-8)
    assert math.isclose(y,ny,abs_tol=1e-8)
    assert math.isclose(z,nz,abs_tol=1e-8)
    # Allow one pixel for host libm/decimal fixture rounding at an integer edge.
    assert abs(int(x*600/z+384)-sx)<=1
    assert abs(int(y*600/z+240)-sy)<=1
assert len(rows)==360
print('Ending projection equations: 360 native stage-4 cases match (1-pixel tolerance)')
