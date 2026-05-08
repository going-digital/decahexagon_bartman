Screen resolution is 320 x 200. Reduced Y resolution is because the game is played in 16:10.

| TV system | Pixel aspect | Resolution | Overscan | Letterbox lines|
|-|-|-|-|-|
|PAL|16:15|320x214|No|42|
|PAL|16:15|352x234|Yes|22|
|PAL|16:15|376x250|Yes|6|
|NTSC|5:6|320x166|No|34|
|NTSC|5:6|352x184|Yes|16|
|NTSC|5:6|376x196|Yes|4|
|Square|1:1|320x200|No|56|
|Square|1:1|352x220|Yes|36|
|Square|1:1|376x234|Yes|22|


# DMA budget

227 DMA per scanline

|Transfers|Purpose|
|-|-|
|20|Screen rendering (2 bitplanes)|
|20|Screen clearing (2 bitplanes)|
|40|Fill read operations (2 bitplanes)|
|40|Fill write operations (2 bitplanes)|
|127|Free for line drawing and sound DMA|


# Line to edge of screen

## Intersecting top edge

* y1 = SCREEN
* x1 = SCREEN

# Vector routines

## Line clipping



## Wall positions from video

| Frame hitting centre | Walls clockwise from player start position|
|-|-|
| 235 - 243 | -W--W- |
| 279 - 287 | W-W-W- |
| 323 - 330 | W-W-W- |
| 374 - 382 | -W-W-W |
| 426 - 434 | W-W-W- |
| 478 - 486 | -W-W-W |
| 530 - 537 | WWW-WW |
| 583 - 590 | -WWWWW |
| 635 - 641 | WWW-WW |
| 703 - 710 | W-W-W- |
| 756 - 762 | -WWW-W |
| 807 - 814 |
| 858 - 866 |
| 911 - 919 |
| 963 - 971 |
| 1029 - 1037 |
| 1084 - 1092 |
| 1133 - 1140 |
| 1181 - 1188 |
| 1229 - 1236 |
| 1291 - 1302 | Transition to pentagon |
| 1352 - 1358 | (transitioned segment was not wall)
| 1477 - 1484 |
| 1524 - 1530 |
| 1539 - 1546 |
| 1570 - 1576 |
| 1585 - 1591 |
| 1624 - 1634 | Transition to hexagon |
| 1684 - 1690 |
| 1699 - 1705 |
| 1715 - 1720 |
| 1810 - 1815 |
| 1824 - 1831 |
| 1855 - 1861 |
| 1870 - 1877 |
| 1901 - 1907 |
| 1947 - 1954 |
| 1993 - 2000 |
| 2054 - 2061 |
| 2070 - 2076 |
| 2100 - 2110 | Transition to pentagon
| 2160 - 2167 |
| 2286 - 2292 |
| 2301 - 2308 |
| 2332 - 2338 |
| 2378 - 2385 |
| 2393 - 2400 |
| 2424 - 2434 | Transition to square
| 2483 - 2489 |
| 2609 - 2615 |
| 2625 - 2630 |
| 2661 - 2666 |
| 2705 - 2710 |
| 2720 - 2726 |
| 2758 - 2767 | Transition to pentagon
| 2817 - 2820 |
| 2829 - 2836 |
| 2843 - 2849 |
| 2928 / 2940 / 2955 / 2962 / 2969 / 2975 / 2982 |
| 3010 - 3015 |
| 3044 - 3050 |
| 3078 - 3084 |
| 3119 / 3131 / 3146 / 3153 / 3160 / 3166 / 3173 |
| 3120 - 3211 | Transition to hexagon
| 3258 - 3264 |
| 3272 - 3277 |
| 3285 - 3291 |
| 3369 - 3375 |
| 3410 - 3416 |
| 3451 - 3457 |
| 3492 - 3498 |
| 3543 | 60 seconds. Trigger BEST to move, colour change
| 3546 / 3552 / 3571 / 3580 / 3590 / 3592 / 3612 / 3637 / 3645 |
| 3678 / 3682 / 

435, 620: Start frame at top

It appears walls are usually 9 frames thick. Note that the time is in seconds:frames, not seconds:fraction

# Blue noise dither links
https://www.imaging.org/common/uploaded%20files/pdfs/Papers/1999/RP-0-93/1786.pdf
https://bartwronski.com/2020/04/26/optimizing-blue-noise-dithering-backpropagation-through-fourier-transform-and-sorting/
https://psychopath.io/post/2022_07_24_owen_scrambling_based_dithered_blue_noise_sampling
https://www.wedesoft.de/software/2022/09/21/blue-noise-dithering/
https://abau.io/blog/blue_noise_dithering/
https://cv.ulichney.com/papers/1988-blue-noise.pdf
https://iliyan.com/publications/DitheredSampling


# Would table assisted long multiplication be faster?

If we had a table of 0x00, 0x10, .. 0xF0 x multiplicand and 0x00, 0x01, .. 0x0F x multiplicand, could do 8 bit multiplication with 2 lookups. We already know there are only 12 multiplicands (sin and cos of each angle). Could that speed up the polar conversion?

Alternatively, what about precalculating unit vectors for each angle, then adding them to the total when walking through the polygons. Would take out polar conversion completely.

# Clear out line bugs

* [ ] Try logging line draws to debug channel.
* [ ] Test for missing lines in render. Trigger breakpoint.

# Improved shading
* [ ] Vector engine
  * [ ] Fix the screen flash
    * [ ] Zero length lines?
    * [ ] One pixel length lines?
    * [ ] Are we looking at the correct buffer?
* [ ] Colour phasing
  * [ ] Generate dither pattern
  * [ ] Add bitplanes
  * [ ] Adapt colour palette
  * [ ] Before/after tests
  * [ ] Also test with 1 bitplane

# Blitter optimisation
  * [ ] Build blitter list to run in the next frame
  * [ ] This should keep the blitter 100% occupied at the start of a frame, freeing the blitter later.
  * [ ] Use the skip function to miss every other line from the fill if timing is tight - so the drop to 50% fill is automatic.

# Moving forward

* [ ] Game starts with zooming into walls.
* [ ] Most wall objects are done that way.
* [ ] But can seamlessly switch to pre-recorded setpieces.
  * [ ] Can have more polygons on screen
  * [ ] But frames are fixed.
* [ ] Could precalculate frames ahead - they can be distilled down to a copper list for later rendering.

* Frames are rendered to a copper list.
* The copper list is rasterised to the bitplane 1 frame or more later automatically by the copper and blitter.
  * Copper list assumes a bitplane base address. Might need adapting if calculated well ahead, as base address is only known at runtime.
  * This means latency is likely 3 frames:
    * Decide on frame content, build copperlist.
    * Rasterise frame by running copperlist.
    * Present completed framebuffer to screen.

# Activities each frame

* Present frame C to screen. (DMA driven)
* Run copperlist for frame B. (DMA driven from copper/blitter)
* Generate the copperlist for frame A. (CPU)
  * Note blitter is not available.
* Each frame, swap screen buffers and copperlist addresses.

# Enemies

@2s31f
```
=..=..
```

@3s17f 10s39f
```
=.=.=.
```

@4s02f / 5s50f / 16s19f
```
=.=.=.

.=.=.=
```

@7s39f 13s21f 18s07f
```
=====.

==.===

=====.
```

@11s33f
```
.=.===
```

@12s27f
```
.=====
```

@20s52f hexagon to pentagon transition
```
.=.=== -> =.===
```