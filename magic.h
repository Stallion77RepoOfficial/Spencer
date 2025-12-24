#ifndef MAGIC_H
#define MAGIC_H
#include "defs.h"

extern U64 RookMagic[64];
extern U64 BishopMagic[64];
extern int RookShift[64];
extern int BishopShift[64];

extern U64 RookMask[64], BishopMask[64];
extern U64 *RookAttackPtr[64];
extern U64 *BishopAttackPtr[64];

void init_magics();

inline U64 rook_attacks(int sq, U64 occ) {
  return RookAttackPtr[sq]
                      [((occ & RookMask[sq]) * RookMagic[sq]) >> RookShift[sq]];
}

inline U64 bishop_attacks(int sq, U64 occ) {
  return BishopAttackPtr[sq][((occ & BishopMask[sq]) * BishopMagic[sq]) >>
                             BishopShift[sq]];
}

inline U64 queen_attacks(int sq, U64 occ) {
  return rook_attacks(sq, occ) | bishop_attacks(sq, occ);
}
#endif
