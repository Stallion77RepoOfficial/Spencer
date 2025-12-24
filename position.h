#ifndef POSITION_H
#define POSITION_H

#include "defs.h"
#include <string>

struct Position {
  U64 bb[2][6];
  U64 occ[2];
  U64 all;
  int side;
  int castling;
  int ep;
};

Position parseFEN(std::string fen);

#endif
