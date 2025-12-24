#ifndef MOVEGEN_H
#define MOVEGEN_H

#include "defs.h"
#include "position.h"

struct MoveList {
  Move moves[256];
  int count;
};

template <int Side> U64 attackers_from(const Position &p, int sq, U64 occ);
template <int Us> uint64_t perft(Position &p, int depth);

void generate_moves(const Position &p, MoveList &list);

#endif
