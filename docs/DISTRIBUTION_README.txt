Hexagon for Amiga

ADF/hexagon.adf
  Bootable floppy edition. Write it to an Amiga floppy or mount it in an emulator.
  The distributed image has blank save slots. Use a writable working copy to save
  progress, and retain that copy when installing a newer distribution.

WHDLoad/Hexagon/
  Copy this entire directory to your Amiga hard disk. WHDLoad 20 is required.
  Launch Hexagon-1, -2 or -3 from Workbench for one, two or three decoded
  soundtrack cache slots. See the included README.md for memory requirements,
  Shell launch commands, save handling and validation limits.
  Keep existing data/save-a and data/save-b when updating an installation.

Both editions use the same PC-derived soundtracks. Personal saves, emulator
configurations, ROMs, AmigaOS and developer SDK files are not included.
The manifests record file sizes and SHA-256 hashes.

Progress stays in memory during retries. Return to level selection with Escape
to save your scores and unlocks before switching off or forcibly quitting.

The ADF keeps using the physical drive it booted from (DF0 through DF3).
External-drive boot still requires firmware or a boot selector that supports it.
