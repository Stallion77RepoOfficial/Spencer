#ifndef NEON_OPT_H
#define NEON_OPT_H

#include "defs.h"
#include <arm_neon.h>

// NEON-optimized population count for 128-bit vector
inline int popcnt128(uint8x16_t v) { return vaddvq_u8(vcntq_u8(v)); }

// Convert two U64 bitboards to NEON vector
inline uint8x16_t load_128(U64 lo, U64 hi) {
  uint64x2_t v = {lo, hi};
  return vreinterpretq_u8_u64(v);
}

// Convert one U64 to duplicated NEON vector
inline uint8x16_t dup_128(U64 x) {
  uint64x2_t v = vdupq_n_u64(x);
  return vreinterpretq_u8_u64(v);
}

// Bitwise Ops
inline uint8x16_t neon_or(uint8x16_t a, uint8x16_t b) { return vorrq_u8(a, b); }
inline uint8x16_t neon_and(uint8x16_t a, uint8x16_t b) {
  return vandq_u8(a, b);
}
inline uint8x16_t neon_xor(uint8x16_t a, uint8x16_t b) {
  return veorq_u8(a, b);
}
inline uint8x16_t neon_not(uint8x16_t a) { return vmvnq_u8(a); }
// vbicq_u64 not exposed as u8 wrapper usually, but easy to cast
inline uint8x16_t neon_andnot(uint8x16_t a, uint8x16_t b) {
  return vbicq_u8(a, b);
}

// Accumulation Helper (8 -> 16)
inline uint16x8_t neon_acc_8_to_16(uint16x8_t acc, uint8x16_t v) {
  return vpadalq_u8(acc, vcntq_u8(v));
}

// Reverse
inline uint8x16_t neon_rbit_64(uint8x16_t v) {
  // Reverse bytes in 64-bit element, then reverse bits in byte
  return vrbitq_u8(vrev64q_u8(v));
}

// Arithmetic
inline uint8x16_t neon_sub(uint8x16_t a, uint8x16_t b) {
  return vreinterpretq_u8_u64(
      vsubq_u64(vreinterpretq_u64_u8(a), vreinterpretq_u64_u8(b)));
}

#endif
