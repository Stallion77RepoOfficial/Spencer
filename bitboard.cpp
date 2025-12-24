#include "bitboard.h"
#include <cstring>

U64 knightAtt[64], kingAtt[64], pawnAtt[2][64];
U64 LineBB[64][64];
U64 BetweenBB[64][64];

U64 get_rook_mask(int sq) {
  U64 r = 0;
  int rk = sq >> 3, fl = sq & 7;
  for (int i = rk + 1; i < 7; i++)
    r |= 1ULL << (i * 8 + fl);
  for (int i = rk - 1; i > 0; i--)
    r |= 1ULL << (i * 8 + fl);
  for (int i = fl + 1; i < 7; i++)
    r |= 1ULL << (rk * 8 + i);
  for (int i = fl - 1; i > 0; i--)
    r |= 1ULL << (rk * 8 + i);
  return r;
}

U64 get_bishop_mask(int sq) {
  U64 r = 0;
  int rk = sq >> 3, fl = sq & 7;
  for (int i = 1; rk + i < 7 && fl + i < 7; i++)
    r |= 1ULL << ((rk + i) * 8 + fl + i);
  for (int i = 1; rk + i < 7 && fl - i > 0; i++)
    r |= 1ULL << ((rk + i) * 8 + fl - i);
  for (int i = 1; rk - i > 0 && fl + i < 7; i++)
    r |= 1ULL << ((rk - i) * 8 + fl + i);
  for (int i = 1; rk - i > 0 && fl - i > 0; i++)
    r |= 1ULL << ((rk - i) * 8 + fl - i);
  return r;
}

U64 rook_attacks_slow(int sq, U64 block) {
  U64 a = 0;
  int r = sq >> 3, f = sq & 7;
  for (int i = r + 1; i < 8; i++) {
    a |= 1ULL << (i * 8 + f);
    if (block & (1ULL << (i * 8 + f)))
      break;
  }
  for (int i = r - 1; i >= 0; i--) {
    a |= 1ULL << (i * 8 + f);
    if (block & (1ULL << (i * 8 + f)))
      break;
  }
  for (int i = f + 1; i < 8; i++) {
    a |= 1ULL << (r * 8 + i);
    if (block & (1ULL << (r * 8 + i)))
      break;
  }
  for (int i = f - 1; i >= 0; i--) {
    a |= 1ULL << (r * 8 + i);
    if (block & (1ULL << (r * 8 + i)))
      break;
  }
  return a;
}

U64 bishop_attacks_slow(int sq, U64 block) {
  U64 a = 0;
  int r = sq >> 3, f = sq & 7;
  for (int i = 1; r + i < 8 && f + i < 8; i++) {
    a |= 1ULL << ((r + i) * 8 + f + i);
    if (block & (1ULL << ((r + i) * 8 + f + i)))
      break;
  }
  for (int i = 1; r + i < 8 && f - i >= 0; i++) {
    a |= 1ULL << ((r + i) * 8 + f - i);
    if (block & (1ULL << ((r + i) * 8 + f - i)))
      break;
  }
  for (int i = 1; r - i >= 0 && f + i < 8; i++) {
    a |= 1ULL << ((r - i) * 8 + f + i);
    if (block & (1ULL << ((r - i) * 8 + f + i)))
      break;
  }
  for (int i = 1; r - i >= 0 && f - i >= 0; i++) {
    a |= 1ULL << ((r - i) * 8 + f - i);
    if (block & (1ULL << ((r - i) * 8 + f - i)))
      break;
  }
  return a;
}

U64 index_to_u64(int idx, int bits, U64 m) {
  U64 r = 0;
  for (int i = 0; i < bits; i++) {
    int j = poplsb(m);
    if (idx & (1 << i))
      r |= 1ULL << j;
  }
  return r;
}

void init_tables() {
  for (int sq = 0; sq < 64; sq++) {
    int r = sq >> 3, f = sq & 7;
    U64 b = 0;
    int dr[] = {2, 2, 1, 1, -1, -1, -2, -2},
        df[] = {1, -1, 2, -2, 2, -2, 1, -1};
    for (int i = 0; i < 8; i++) {
      int rr = r + dr[i], ff = f + df[i];
      if (rr >= 0 && rr < 8 && ff >= 0 && ff < 8)
        b |= 1ULL << (rr * 8 + ff);
    }
    knightAtt[sq] = b;
    b = 0;
    for (int rr = r - 1; rr <= r + 1; rr++)
      for (int ff = f - 1; ff <= f + 1; ff++)
        if (rr >= 0 && rr < 8 && ff >= 0 && ff < 8 && (rr != r || ff != f))
          b |= 1ULL << (rr * 8 + ff);
    kingAtt[sq] = b;
    pawnAtt[WHITE][sq] = ((r < 7 && f > 0) ? 1ULL << (sq + 7) : 0) |
                         ((r < 7 && f < 7) ? 1ULL << (sq + 9) : 0);
    pawnAtt[BLACK][sq] = ((r > 0 && f > 0) ? 1ULL << (sq - 9) : 0) |
                         ((r > 0 && f < 7) ? 1ULL << (sq - 7) : 0);
  }

  for (int s1 = 0; s1 < 64; s1++) {
    for (int s2 = 0; s2 < 64; s2++) {
      LineBB[s1][s2] = 0;
      BetweenBB[s1][s2] = 0;
      if (s1 == s2)
        continue;
      U64 ra = rook_attacks_slow(s1, 0);
      U64 ba = bishop_attacks_slow(s1, 0);
      if (ra & (1ULL << s2)) {
        LineBB[s1][s2] = (rook_attacks_slow(s1, 0) & rook_attacks_slow(s2, 0)) |
                         (1ULL << s1) | (1ULL << s2);
        BetweenBB[s1][s2] = rook_attacks_slow(s1, 1ULL << s2) &
                            rook_attacks_slow(s2, 1ULL << s1);
      } else if (ba & (1ULL << s2)) {
        LineBB[s1][s2] =
            (bishop_attacks_slow(s1, 0) & bishop_attacks_slow(s2, 0)) |
            (1ULL << s1) | (1ULL << s2);
        BetweenBB[s1][s2] = bishop_attacks_slow(s1, 1ULL << s2) &
                            bishop_attacks_slow(s2, 1ULL << s1);
      }
    }
  }
}
