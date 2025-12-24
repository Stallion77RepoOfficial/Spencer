#include "magic.h"
#include "bitboard.h"
#include <iostream>
#include <random>
#include <vector>

U64 RookMagic[64];
U64 BishopMagic[64];
int RookShift[64] = {52, 53, 53, 53, 53, 53, 53, 52, 53, 54, 54, 54, 54,
                     54, 54, 53, 53, 54, 54, 54, 54, 54, 54, 53, 53, 54,
                     54, 54, 54, 54, 54, 53, 53, 54, 54, 54, 54, 54, 54,
                     53, 53, 54, 54, 54, 54, 54, 54, 53, 53, 54, 54, 54,
                     54, 54, 54, 53, 52, 53, 53, 53, 53, 53, 53, 52};
int BishopShift[64] = {58, 59, 59, 59, 59, 59, 59, 58, 59, 59, 59, 59, 59,
                       59, 59, 59, 59, 59, 57, 57, 57, 57, 59, 59, 59, 59,
                       57, 55, 55, 57, 59, 59, 59, 59, 57, 55, 55, 57, 59,
                       59, 59, 59, 57, 57, 57, 57, 59, 59, 59, 59, 59, 59,
                       59, 59, 59, 59, 58, 59, 59, 59, 59, 59, 59, 58};

U64 RookMask[64], BishopMask[64];
U64 *RookAttackPtr[64];
U64 *BishopAttackPtr[64];

std::vector<U64> rookTable;
std::vector<U64> bishopTable;

U64 random_u64() {
  static U64 s = 1804289383;
  U64 x = s;
  x ^= x << 13;
  x ^= x >> 7;
  x ^= x << 17;
  s = x;
  return x;
}

U64 random_u64_fewbits() { return random_u64() & random_u64() & random_u64(); }

U64 find_magic(int sq, int m, bool is_bishop) {
  U64 mask = is_bishop ? BishopMask[sq] : RookMask[sq];
  int bits = popcnt(mask);
  int size = 1 << bits;
  U64 occ[4096];
  U64 att[4096];
  U64 used[4096];

  for (int i = 0; i < size; i++) {
    occ[i] = index_to_u64(i, bits, mask);
    att[i] = is_bishop ? bishop_attacks_slow(sq, occ[i])
                       : rook_attacks_slow(sq, occ[i]);
  }

  for (int k = 0; k < 100000000; k++) {
    U64 magic = random_u64_fewbits();
    if (popcnt((mask * magic) & 0xFF00000000000000ULL) < 6)
      continue;

    for (int i = 0; i < 4096; i++)
      used[i] = 0;
    bool fail = false;

    for (int i = 0; i < size; i++) {
      int idx = (int)((occ[i] * magic) >> (64 - m));
      if (used[idx] == 0)
        used[idx] = att[i];
      else if (used[idx] != att[i]) {
        fail = true;
        break;
      }
    }
    if (!fail)
      return magic;
  }
  std::cerr << "Failed to find magic!" << std::endl;
  return 0;
}

void init_magics() {
  init_tables();

  for (int sq = 0; sq < 64; sq++) {
    RookMask[sq] = get_rook_mask(sq);
    BishopMask[sq] = get_bishop_mask(sq);
  }

  rookTable.resize(0x100000);
  bishopTable.resize(0x1480);

  std::cout << "Generating Magics... " << std::flush;
  for (int sq = 0; sq < 64; sq++) {
    RookMagic[sq] = find_magic(sq, 64 - RookShift[sq], false);
    BishopMagic[sq] = find_magic(sq, 64 - BishopShift[sq], true);
  }
  std::cout << "Done." << std::endl;

  int roffset = 0;
  for (int sq = 0; sq < 64; sq++) {
    RookAttackPtr[sq] = &rookTable[roffset];
    U64 mask = RookMask[sq];
    int bits = popcnt(mask);
    int size = 1 << bits;
    for (int i = 0; i < size; i++) {
      U64 occ = index_to_u64(i, bits, mask);
      int idx = (int)((occ * RookMagic[sq]) >> RookShift[sq]);
      RookAttackPtr[sq][idx] = rook_attacks_slow(sq, occ);
    }
    roffset += (1 << (64 - RookShift[sq]));
  }

  int boffset = 0;
  for (int sq = 0; sq < 64; sq++) {
    BishopAttackPtr[sq] = &bishopTable[boffset];
    U64 mask = BishopMask[sq];
    int bits = popcnt(mask);
    int size = 1 << bits;
    for (int i = 0; i < size; i++) {
      U64 occ = index_to_u64(i, bits, mask);
      int idx = (int)((occ * BishopMagic[sq]) >> BishopShift[sq]);
      BishopAttackPtr[sq][idx] = bishop_attacks_slow(sq, occ);
    }
    boffset += (1 << (64 - BishopShift[sq]));
  }
}
