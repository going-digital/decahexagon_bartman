# Baseline visibility

The reference is PC graphics mode 0 (768 x 480), steady gameplay, zero pulse
and no camera tilt or transient effects. Native `gameclass::initfov()` at
`0x100009970` gives fx=665.107486138, fy=707.017178437 and depth=850.
`transformscene()` projects x*fx/depth and y*fy/depth in this baseline.
Resizing to 320 x 200 gives scales 0.3260330814 and 0.3465770483.

`view_scale.h` stores the residual Q14 factors after gameplay's existing
0.5 zoom. `init_tables()` applies them once to independent X/Y tables.
Both cached wall directions and polar player/spoke directions use those
tables; the old 0.75 vertical squeeze is removed. The extra X table costs
2 KiB of ordinary memory, with no extra per-vertex multiplication.

The centre-to-edge world extents are now approximately 490.769 x 288.527,
versus PC 490.748 x 288.536. Previously they were 320 x 266.667.
This is baseline framing equivalence, not implementation of PC perspective
tilt, death-camera effects, or exact rasterization. Pixel truncation and the
existing approximate sine table still affect individual edge pixels.

`make test-view-scale` compares the quantized projection with the independent
PC constants over 1,024 directions and radii 0..600. Its error bound excludes
the existing trigonometric approximation. The player atlas test checks the
new scale across all 65,536 angles. Target `pc_trig_checks()` verifies cardinal
table scales and both actual assembly projection paths.

Local PC evidence: `scratchpad/decompile_gfx2.txt` (initgfx mode 0),
`scratchpad/decompile_gfx1.txt` (compose3dframe), and
`scratchpad/pc_verification/evidence/fresh/gameclass_transformscene_10000ab90.c`.
The native FOV probe and original audit are in `scratchpad/visibility_audit/`.
