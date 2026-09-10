#pragma once

// Shared low-level Amiga hardware access.
// Pulls in the register struct, common hardware bit definitions and the
// pointers to the custom chips / CIAs (defined in system.c).

#include <exec/types.h>
#include "support/gcc8_c_support.h"
#include "custom2.h"
#include <hardware/dmabits.h>
#include <hardware/intbits.h>
#include <hardware/adkbits.h>
#include <hardware/blit.h>
#include <hardware/cia.h>
#include "paulabits.h"

extern struct Custom *custom;
extern struct CIA *ciaa;
extern struct CIA *ciab;
