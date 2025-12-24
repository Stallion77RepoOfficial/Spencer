#ifndef DEFS_H
#define DEFS_H

#include <cstdint>

using U64 = uint64_t;

enum { WHITE, BLACK };
enum { PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING };

// Move encoding:
// 0-5: To
// 6-11: From
// 12-14: Flags
using Move = uint16_t;

enum MoveFlags {
  QUIET = 0,
  DOUBLE_PUSH = 1,
  CAPTURE = 2, // Generic capture
  EP_CAPTURE = 3,
  CASTLE = 4, // Castling
  PROMO_N = 5,
  PROMO_B = 6,
  PROMO_R = 7,
  PROMO_Q = 8
};

inline Move make_move_enc(int f, int t, int flag) {
  return (f << 6) | t | (flag << 12);
}

inline int move_from(Move m) { return (m >> 6) & 0x3F; }
inline int move_to(Move m) { return m & 0x3F; }
inline int move_flag(Move m) { return (m >> 12) & 0x7; }

#endif
