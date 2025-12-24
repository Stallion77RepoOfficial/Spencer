#ifndef BITBOARD_H
#define BITBOARD_H

#include "defs.h"
#include <random>

inline int poplsb(U64 &b) {
  int s = __builtin_ctzll(b);
  b &= b - 1;
  return s;
}

inline int popcnt(U64 b) { return __builtin_popcountll(b); }

extern U64 knightAtt[64];
extern U64 kingAtt[64];
extern U64 pawnAtt[2][64];
extern U64 LineBB[64][64];
extern U64 BetweenBB[64][64];

void init_tables();

U64 get_rook_mask(int sq);
U64 get_bishop_mask(int sq);
U64 rook_attacks_slow(int sq, U64 block);
U64 bishop_attacks_slow(int sq, U64 block);
U64 index_to_u64(int idx, int bits, U64 m);

#endif
