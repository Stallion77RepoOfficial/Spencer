#ifndef SPENCER_H
#define SPENCER_H

#include "bitboard.h"
#include "defs.h"
#include "magic.h"
#include "movegen.h"
#include "position.h"

// Initialize everything (calls init_magics which calls init_tables)
inline void spencer_init() { init_magics(); }

#endif
