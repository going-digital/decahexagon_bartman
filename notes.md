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
