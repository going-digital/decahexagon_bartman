* Smooth colour transitions
  * [ ] Add a bitplane of 1/3rd set pixel blue noise. Use this to give 1.58 bits extra precision on the palette to reduce colour jumps. So from 16 graduations to 46, making the colour transition smoother.
  * [ ] Add another dither bitplane. Have an ordered blue noise dither pattern, with ratios 1,2,4,8 of 15. This should give 3.9 bits of dither, so nearly 8 bit colour resolution!
  * [ ] Offset the dither pattern every frame.
  * [ ] Python to build these dither patterns.

* Graphics level up
  * Need more than one rendering engine.
    * Real time polar filled polygons
    * Filled polygons render to copper list.
      * Needed to render complex scenes.
      * [x] Implement cls
      * [x] Implement onedot lines
      * [x] Implement clipped onedot lines
      * [x] Implement fill
      * [ ] Try render-to-copper as an optimisation
      * Could be precalculated or loaded off disk. Need to know target bitplane address ahead of time which might make loading off disk tricky unless entire memory map is fixed.
        * Can't be loaded off disk - address of screenbuffers is not known at compile time.
      * Precalculation requires threading - we've now got a lot to juggle per frame.
        * Real time rendering
        * Precalculating next boss scene
        * Possibly loading from disk
        * Possibly sample rendering
        * Tracker playing
        * Video live effects
        * [ ] Start to integrate Protothreads from Trident^Fairlight
        * [ ] Gameplay frame now really needs to be on interrupt so it can guarantee interrupting other tasks cleanly.
  * [ ] Implement motion blur by presenting old frames.
  * [ ] Look at alternative renderers. Would be nice to flip complete render techniques.
    * [ ] What other beat game tropes are there?
      * Fight on the beat - Beat Sabre
      * Fire on the beat - Rez
      * Jump on the beat - several including Geometry Wars

* Music level up
  * [ ] Frame interrupt driven LSP
  * MusyX looks like a useful concept. Read manual.
