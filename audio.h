#pragma once

// Music playback. Two mutually exclusive back ends selected in config.h:
//   MUSIC_LSP - LightSpeed Player (CIA timed)
//   MUSIC     - ThePlayer 6.1a (VBL timed, driven from interruptHandler)

#include "config.h"

// Doynax LZ depacker (returns end of output data; input must be 16-bit aligned).
void* doynaxdepack(const void* input, void* output);

#ifdef MUSIC_LSP
int p61Init(void);
int p61Music(void);
int p61End(void);
#endif

#ifdef MUSIC
extern const void* module;
int p61Init(const void* module);
void p61Music(void);
void p61End(void);
#endif
