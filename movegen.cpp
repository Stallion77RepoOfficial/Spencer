#include "movegen.h"
#include "bitboard.h"
#include "magic.h"
#include "neon_opt.h"
#include <cstring>

struct Board {
  U64 bb[12];
  U64 occ[2];
};

template <int Side> inline U64 attackers_to(const Board &b, int sq, U64 occ) {
  constexpr int Them = Side ^ 1;
  U64 sO = Side * 6;
  return (pawnAtt[Them][sq] & b.bb[sO + PAWN]) |
         (knightAtt[sq] & b.bb[sO + KNIGHT]) | (kingAtt[sq] & b.bb[sO + KING]) |
         (rook_attacks(sq, occ) & (b.bb[sO + ROOK] | b.bb[sO + QUEEN])) |
         (bishop_attacks(sq, occ) & (b.bb[sO + BISHOP] | b.bb[sO + QUEEN]));
}

inline int update_cr(int cr, int us, int f, int t, int pt, int c) {
  if (pt == KING)
    cr &= (us == WHITE ? ~3 : ~12);
  if (pt == ROOK) {
    if (us == WHITE) {
      if (f == 0)
        cr &= ~2;
      else if (f == 7)
        cr &= ~1;
    } else {
      if (f == 56)
        cr &= ~8;
      else if (f == 63)
        cr &= ~4;
    }
  }
  if (c == ROOK) {
    if (us == BLACK) {
      if (t == 0)
        cr &= ~2;
      else if (t == 7)
        cr &= ~1;
    } else {
      if (t == 56)
        cr &= ~8;
      else if (t == 63)
        cr &= ~4;
    }
  }
  return cr;
}

inline void apply_move(Board &n, int us, int f, int t, int pt, int pr, int ct,
                       int cs) {
  int UsO = us * 6;
  U64 from = 1ULL << f, to = 1ULL << t;

  n.bb[UsO + pt] ^= from;
  n.occ[us] ^= from;

  int final_pt = (pr != -1) ? pr : pt;
  n.bb[UsO + final_pt] ^= to;
  n.occ[us] ^= to;

  if (ct != -1) {
    int Them = us ^ 1;
    int ThO = Them * 6;
    U64 cap = 1ULL << cs;
    n.bb[ThO + ct] ^= cap;
    n.occ[Them] ^= cap;
  }
}

inline int get_captured_piece(const Board &b, int t, int Them) {
  int ThO = Them * 6;
  U64 mask = 1ULL << t;
  if (b.bb[ThO + PAWN] & mask)
    return PAWN;
  if (b.bb[ThO + KNIGHT] & mask)
    return KNIGHT;
  if (b.bb[ThO + BISHOP] & mask)
    return BISHOP;
  if (b.bb[ThO + ROOK] & mask)
    return ROOK;
  if (b.bb[ThO + QUEEN] & mask)
    return QUEEN;
  return -1;
}

template <int Us, int CR, bool HasEP>
uint64_t perft_t(Board &b, int depth, int ep_sq);

template <int Us, bool Ep>
inline uint64_t switch_cr_t(Board &b, int depth, int cr, int ep_sq) {
  switch (cr) {
  case 0:
    return perft_t<Us, 0, Ep>(b, depth, ep_sq);
  case 1:
    return perft_t<Us, 1, Ep>(b, depth, ep_sq);
  case 2:
    return perft_t<Us, 2, Ep>(b, depth, ep_sq);
  case 3:
    return perft_t<Us, 3, Ep>(b, depth, ep_sq);
  case 4:
    return perft_t<Us, 4, Ep>(b, depth, ep_sq);
  case 5:
    return perft_t<Us, 5, Ep>(b, depth, ep_sq);
  case 6:
    return perft_t<Us, 6, Ep>(b, depth, ep_sq);
  case 7:
    return perft_t<Us, 7, Ep>(b, depth, ep_sq);
  case 8:
    return perft_t<Us, 8, Ep>(b, depth, ep_sq);
  case 9:
    return perft_t<Us, 9, Ep>(b, depth, ep_sq);
  case 10:
    return perft_t<Us, 10, Ep>(b, depth, ep_sq);
  case 11:
    return perft_t<Us, 11, Ep>(b, depth, ep_sq);
  case 12:
    return perft_t<Us, 12, Ep>(b, depth, ep_sq);
  case 13:
    return perft_t<Us, 13, Ep>(b, depth, ep_sq);
  case 14:
    return perft_t<Us, 14, Ep>(b, depth, ep_sq);
  case 15:
    return perft_t<Us, 15, Ep>(b, depth, ep_sq);
  }
  return 0;
}

template <int Us, int CR, bool HasEP>
uint64_t perft_t(Board &b, int depth, int ep_sq) {
  if (depth == 0)
    return 1;

  constexpr int Them = Us ^ 1, Up = (Us == WHITE ? 8 : -8), UsO = Us * 6,
                ThO = Them * 6;
  constexpr U64 PRo =
      (Us == WHITE ? 0xFF00000000000000ULL : 0x00000000000000FFULL);
  constexpr U64 R3 =
      (Us == WHITE ? 0x0000000000FF0000ULL : 0x0000FF0000000000ULL);

  int ksq = __builtin_ctzll(b.bb[UsO + KING]);
  U64 all = b.occ[0] | b.occ[1];
  U64 checkrs = attackers_to<Them>(b, ksq, all);
  int nC = popcnt(checkrs);

  U64 pinned = 0, pinner = ((rook_attacks(ksq, b.occ[Them]) &
                             (b.bb[ThO + ROOK] | b.bb[ThO + QUEEN])) |
                            (bishop_attacks(ksq, b.occ[Them]) &
                             (b.bb[ThO + BISHOP] | b.bb[ThO + QUEEN])));
  while (pinner) {
    int s = poplsb(pinner);
    U64 blk = BetweenBB[ksq][s] & b.occ[Us];
    if (popcnt(blk) == 1)
      pinned |= blk;
  }

  uint64_t nodes = 0;

  if (nC > 1) {
    U64 km = kingAtt[ksq] & ~b.occ[Us];
    while (km) {
      int t = poplsb(km);
      if (!attackers_to<Them>(b, t, (all ^ (1ULL << ksq)) | (1ULL << t))) {
        if (depth == 1) {
          nodes++;
          continue;
        }

        int c = get_captured_piece(b, t, Them);
        Board n = b;
        apply_move(n, Us, ksq, t, KING, -1, c, t);
        nodes += switch_cr_t<Them, false>(
            n, depth - 1, update_cr(CR, Us, ksq, t, KING, c), -1);
      }
    }
    return nodes;
  }

  U64 tgt = (nC == 1) ? (checkrs | BetweenBB[ksq][__builtin_ctzll(checkrs)])
                      : ~b.occ[Us];
  auto pok = [&](int f, int t) {
    return !(pinned & (1ULL << f)) || (LineBB[ksq][f] & (1ULL << t));
  };

  if (depth == 1 && nC == 0 && pinned == 0) {
    U64 p1 = (Us == WHITE ? (b.bb[UsO + PAWN] << 8) : (b.bb[UsO + PAWN] >> 8)) &
             ~all;
    U64 p1t = p1 & tgt, p1pr = p1t & PRo, p1q = p1t & ~PRo;
    nodes += popcnt(p1q) + popcnt(p1pr) * 4;
    nodes += popcnt((Us == WHITE ? ((p1 & R3) << 8) : ((p1 & R3) >> 8)) & ~all &
                    tgt);

    U64 p = b.bb[UsO + PAWN];
    // NEON Pawn Captures
    // White: ((P & ~FileA) << 7), ((P & ~FileH) << 9)
    // Black: ((P & ~FileH) >> 7), ((P & ~FileA) >> 9)

    const U64 FileA = 0x0101010101010101ULL;
    const U64 FileH = 0x8080808080808080ULL;

    U64 m0, m1;
    int64_t s0, s1;

    if (Us == WHITE) {
      m0 = ~FileA;
      s0 = 7;
      m1 = ~FileH;
      s1 = 9;
    } else {
      m0 = ~FileH;
      s0 = -7;
      m1 = ~FileA;
      s1 = -9;
    }

    uint64x2_t v_p = vdupq_n_u64(p);
    uint64x2_t v_mask = {m0, m1};
    int64x2_t v_shift = {s0, s1};
    uint64x2_t v_them = vdupq_n_u64(b.occ[Them] & tgt);

    // (P & Mask)
    uint64x2_t v_src = vandq_u64(v_p, v_mask);
    // << Shift
    uint64x2_t v_att = vshlq_u64(v_src, v_shift);
    // & Them & Tgt
    uint64x2_t v_caps = vandq_u64(v_att, v_them);

    // Count
    // Note: Splitting by Promotion Rank is tricky in vector.
    // But bulk counting separates Promo logic: `popcnt(c & ~PRo) + popcnt(c &
    // PRo) * 4`.

    uint64x2_t v_pro = vdupq_n_u64(PRo);

    // Normal Captures: v_caps & ~PRo
    // vbicq_u64(a, b) -> a & ~b
    uint64x2_t v_norm = vbicq_u64(v_caps, v_pro);
    nodes += popcnt128(vreinterpretq_u8_u64(v_norm));

    // Promo Captures: v_caps & PRo
    uint64x2_t v_pcap = vandq_u64(v_caps, v_pro);
    nodes += popcnt128(vreinterpretq_u8_u64(v_pcap)) * 4;

    if (HasEP) {
      U64 ep_p = pawnAtt[Them][ep_sq] & b.bb[UsO + PAWN];
      while (ep_p) {
        int f = poplsb(ep_p), cs = ep_sq - Up;
        if (!(attackers_to<Them>(b, ksq,
                                 (all ^ (1ULL << f) ^ (1ULL << cs)) |
                                     (1ULL << ep_sq)) &
              (b.occ[Them] ^ (1ULL << cs))))
          nodes++;
      }
    }

    U64 kn = b.bb[UsO + KNIGHT];
    while (kn)
      nodes += popcnt(knightAtt[poplsb(kn)] & tgt);

    for (int t = BISHOP; t <= QUEEN; t++) {
      U64 pce = b.bb[UsO + t];
      while (pce) {
        int f = poplsb(pce);
        nodes += popcnt((t == BISHOP ? bishop_attacks(f, all)
                                     : (t == ROOK ? rook_attacks(f, all)
                                                  : queen_attacks(f, all))) &
                        tgt);
      }
    }

    U64 km = kingAtt[ksq] & ~b.occ[Us];
    while (km) {
      int t = poplsb(km);
      if (!attackers_to<Them>(b, t, (all ^ (1ULL << ksq)) | (1ULL << t)))
        nodes++;
    }

    if (CR) {
      if (Us == WHITE) {
        if ((CR & 1) && !(all & 0x60ULL) && !attackers_to<BLACK>(b, 5, all) &&
            !attackers_to<BLACK>(b, 6, all))
          nodes++;
        if ((CR & 2) && !(all & 0xEULL) && !attackers_to<BLACK>(b, 3, all) &&
            !attackers_to<BLACK>(b, 2, all))
          nodes++;
      } else {
        if ((CR & 4) && !(all & 0x6000000000000000ULL) &&
            !attackers_to<WHITE>(b, 61, all) &&
            !attackers_to<WHITE>(b, 62, all))
          nodes++;
        if ((CR & 8) && !(all & 0x0E00000000000000ULL) &&
            !attackers_to<WHITE>(b, 59, all) &&
            !attackers_to<WHITE>(b, 58, all))
          nodes++;
      }
    }
    return nodes;
  }

  U64 p1 =
      (Us == WHITE ? (b.bb[UsO + PAWN] << 8) : (b.bb[UsO + PAWN] >> 8)) & ~all;
  U64 p1t = p1 & tgt, p1pr = p1t & PRo, p1q = p1t & ~PRo;

  while (p1q) {
    int t = poplsb(p1q), f = t - Up;
    if (pok(f, t)) {
      if (depth == 1) {
        nodes++;
        continue;
      }
      Board n = b;
      apply_move(n, Us, f, t, PAWN, -1, -1, t);
      nodes += perft_t<Them, CR, false>(n, depth - 1, -1);
    }
  }

  while (p1pr) {
    int t = poplsb(p1pr), f = t - Up;
    if (pok(f, t)) {
      if (depth == 1) {
        nodes += 4;
        continue;
      }
      for (int pr = KNIGHT; pr <= QUEEN; pr++) {
        Board n = b;
        apply_move(n, Us, f, t, PAWN, pr, -1, t);
        nodes += perft_t<Them, CR, false>(n, depth - 1, -1);
      }
    }
  }

  U64 p2 = (Us == WHITE ? ((p1 & R3) << 8) : ((p1 & R3) >> 8)) & ~all & tgt;
  while (p2) {
    int t = poplsb(p2), f = t - 2 * Up;
    if (pok(f, t)) {
      if (depth == 1) {
        nodes++;
        continue;
      }
      Board n = b;
      apply_move(n, Us, f, t, PAWN, -1, -1, t);
      nodes += perft_t<Them, CR, true>(n, depth - 1, t - Up);
    }
  }

  U64 pcp = b.bb[UsO + PAWN];
  while (pcp) {
    int f = poplsb(pcp);
    U64 caps = pawnAtt[Us][f] & b.occ[Them] & tgt;
    while (caps) {
      int t = poplsb(caps);
      if (!pok(f, t))
        continue;

      int c = get_captured_piece(b, t, Them);

      if ((1ULL << t) & PRo) {
        if (depth == 1) {
          nodes += 4;
          continue;
        }
        for (int pr = KNIGHT; pr <= QUEEN; pr++) {
          Board n = b;
          apply_move(n, Us, f, t, PAWN, pr, c, t);
          nodes += switch_cr_t<Them, false>(
              n, depth - 1, update_cr(CR, Us, f, t, PAWN, c), -1);
        }
      } else {
        if (depth == 1) {
          nodes++;
          continue;
        }
        Board n = b;
        apply_move(n, Us, f, t, PAWN, -1, c, t);
        nodes += switch_cr_t<Them, false>(n, depth - 1,
                                          update_cr(CR, Us, f, t, PAWN, c), -1);
      }
    }
  }

  if (HasEP) {
    U64 ep_p = pawnAtt[Them][ep_sq] & b.bb[UsO + PAWN];
    while (ep_p) {
      int f = poplsb(ep_p), cs = ep_sq - Up;
      if (nC == 1 && !((1ULL << cs) & tgt) && !((1ULL << ep_sq) & tgt))
        continue;

      if (!(attackers_to<Them>(
                b, ksq, (all ^ (1ULL << f) ^ (1ULL << cs)) | (1ULL << ep_sq)) &
            (b.occ[Them] ^ (1ULL << cs)))) {
        if (depth == 1) {
          nodes++;
          continue;
        }
        Board n = b;
        apply_move(n, Us, f, ep_sq, PAWN, -1, PAWN, cs);
        nodes += perft_t<Them, CR, false>(n, depth - 1, -1);
      }
    }
  }

  U64 kn = b.bb[UsO + KNIGHT] & ~pinned;
  while (kn) {
    int f = poplsb(kn);
    U64 a = knightAtt[f] & tgt;
    while (a) {
      int t = poplsb(a);
      if (depth == 1) {
        nodes++;
        continue;
      }

      int c = get_captured_piece(b, t, Them);
      Board n = b;
      apply_move(n, Us, f, t, KNIGHT, -1, c, t);
      nodes += switch_cr_t<Them, false>(n, depth - 1,
                                        update_cr(CR, Us, f, t, KNIGHT, c), -1);
    }
  }

  for (int pt = BISHOP; pt <= QUEEN; pt++) {
    U64 pce = b.bb[UsO + pt];
    while (pce) {
      int f = poplsb(pce);
      U64 a = (pt == BISHOP ? bishop_attacks(f, all)
                            : (pt == ROOK ? rook_attacks(f, all)
                                          : queen_attacks(f, all))) &
              tgt;
      if (pinned & (1ULL << f))
        a &= LineBB[ksq][f];

      while (a) {
        int t = poplsb(a);
        if (depth == 1) {
          nodes++;
          continue;
        }

        int c = get_captured_piece(b, t, Them);
        Board n = b;
        apply_move(n, Us, f, t, pt, -1, c, t);
        nodes += switch_cr_t<Them, false>(n, depth - 1,
                                          update_cr(CR, Us, f, t, pt, c), -1);
      }
    }
  }

  U64 km = kingAtt[ksq] & ~b.occ[Us];
  while (km) {
    int t = poplsb(km);
    if (!attackers_to<Them>(b, t, (all ^ (1ULL << ksq)) | (1ULL << t))) {
      if (depth == 1) {
        nodes++;
        continue;
      }

      int c = get_captured_piece(b, t, Them);
      Board n = b;
      apply_move(n, Us, ksq, t, KING, -1, c, t);
      nodes += switch_cr_t<Them, false>(n, depth - 1,
                                        update_cr(CR, Us, ksq, t, KING, c), -1);
    }
  }

  if (nC == 0 && CR) {
    if (Us == WHITE) {
      if ((CR & 1) && !(all & 0x60ULL) && !attackers_to<BLACK>(b, 5, all) &&
          !attackers_to<BLACK>(b, 6, all)) {
        if (depth == 1)
          nodes++;
        else {
          Board n = b;
          n.bb[UsO + KING] ^= 0x50ULL;
          n.bb[UsO + ROOK] ^= 0xA0ULL;
          n.occ[0] ^= 0xF0ULL;
          nodes += perft_t<BLACK, (CR & ~3), false>(n, depth - 1, -1);
        }
      }
      if ((CR & 2) && !(all & 0xEULL) && !attackers_to<BLACK>(b, 3, all) &&
          !attackers_to<BLACK>(b, 2, all)) {
        if (depth == 1)
          nodes++;
        else {
          Board n = b;
          n.bb[UsO + KING] ^= 0x14ULL;
          n.bb[UsO + ROOK] ^= 0x09ULL;
          n.occ[0] ^= 0x1DULL;
          nodes += perft_t<BLACK, (CR & ~3), false>(n, depth - 1, -1);
        }
      }
    } else {
      if ((CR & 4) && !(all & 0x6000000000000000ULL) &&
          !attackers_to<WHITE>(b, 61, all) &&
          !attackers_to<WHITE>(b, 62, all)) {
        if (depth == 1)
          nodes++;
        else {
          Board n = b;
          n.bb[UsO + KING] ^= 0x5000000000000000ULL;
          n.bb[UsO + ROOK] ^= 0xA000000000000000ULL;
          n.occ[1] ^= 0xF000000000000000ULL;
          nodes += perft_t<WHITE, (CR & ~12), false>(n, depth - 1, -1);
        }
      }
      if ((CR & 8) && !(all & 0x0E00000000000000ULL) &&
          !attackers_to<WHITE>(b, 59, all) &&
          !attackers_to<WHITE>(b, 58, all)) {
        if (depth == 1)
          nodes++;
        else {
          Board n = b;
          n.bb[UsO + KING] ^= 0x1400000000000000ULL;
          n.bb[UsO + ROOK] ^= 0x0900000000000000ULL;
          n.occ[1] ^= 0x1D00000000000000ULL;
          nodes += perft_t<WHITE, (CR & ~12), false>(n, depth - 1, -1);
        }
      }
    }
  }

  return nodes;
}

template <int Us> uint64_t perft(Position &p, int depth) {
  Board b;
  for (int i = 0; i < 12; i++)
    b.bb[i] = p.bb[i / 6][i % 6];
  b.occ[0] = p.occ[0];
  b.occ[1] = p.occ[1];

  if (p.ep != -1)
    return switch_cr_t<Us, true>(b, depth, p.castling, p.ep);
  else
    return switch_cr_t<Us, false>(b, depth, p.castling, -1);
}

template <int Us> void generate(Position &p, MoveList &list) {
  Board b;
  for (int i = 0; i < 12; i++)
    b.bb[i] = p.bb[i / 6][i % 6];
  b.occ[0] = p.occ[0];
  b.occ[1] = p.occ[1];

  constexpr int Them = Us ^ 1, Up = (Us == WHITE ? 8 : -8), UsO = Us * 6,
                ThO = Them * 6;
  constexpr U64 PRo =
      (Us == WHITE ? 0xFF00000000000000ULL : 0x00000000000000FFULL);
  constexpr U64 R3 =
      (Us == WHITE ? 0x0000000000FF0000ULL : 0x0000FF0000000000ULL);

  int ksq = __builtin_ctzll(b.bb[UsO + KING]);
  U64 all = b.occ[0] | b.occ[1];
  U64 checkrs = attackers_to<Them>(b, ksq, all);
  int nC = popcnt(checkrs);

  U64 pinned = 0, pinner = ((rook_attacks(ksq, b.occ[Them]) &
                             (b.bb[ThO + ROOK] | b.bb[ThO + QUEEN])) |
                            (bishop_attacks(ksq, b.occ[Them]) &
                             (b.bb[ThO + BISHOP] | b.bb[ThO + QUEEN])));
  while (pinner) {
    int s = poplsb(pinner);
    U64 blk = BetweenBB[ksq][s] & b.occ[Us];
    if (popcnt(blk) == 1)
      pinned |= blk;
  }

  U64 tgt = (nC == 1) ? (checkrs | BetweenBB[ksq][__builtin_ctzll(checkrs)])
                      : ~b.occ[Us];
  auto pok = [&](int f, int t) {
    return !(pinned & (1ULL << f)) || (LineBB[ksq][f] & (1ULL << t));
  };

  if (nC > 1) {
    U64 km = kingAtt[ksq] & ~b.occ[Us];
    while (km) {
      int t = poplsb(km);
      if (!attackers_to<Them>(b, t, (all ^ (1ULL << ksq)) | (1ULL << t))) {
        list.moves[list.count++] =
            make_move_enc(ksq, t, (1ULL << t) & b.occ[Them] ? CAPTURE : QUIET);
      }
    }
    return;
  }

  U64 p1 =
      (Us == WHITE ? (b.bb[UsO + PAWN] << 8) : (b.bb[UsO + PAWN] >> 8)) & ~all;
  U64 p1t = p1 & tgt, p1pr = p1t & PRo, p1q = p1t & ~PRo;

  while (p1q) {
    int t = poplsb(p1q), f = t - Up;
    if (pok(f, t))
      list.moves[list.count++] = make_move_enc(f, t, QUIET);
  }
  while (p1pr) {
    int t = poplsb(p1pr), f = t - Up;
    if (pok(f, t)) {
      list.moves[list.count++] = make_move_enc(f, t, PROMO_Q);
      list.moves[list.count++] = make_move_enc(f, t, PROMO_R);
      list.moves[list.count++] = make_move_enc(f, t, PROMO_B);
      list.moves[list.count++] = make_move_enc(f, t, PROMO_N);
    }
  }
  U64 p2 = (Us == WHITE ? ((p1 & R3) << 8) : ((p1 & R3) >> 8)) & ~all & tgt;
  while (p2) {
    int t = poplsb(p2), f = t - 2 * Up;
    if (pok(f, t))
      list.moves[list.count++] = make_move_enc(f, t, DOUBLE_PUSH);
  }
  U64 pcp = b.bb[UsO + PAWN];
  while (pcp) {
    int f = poplsb(pcp);
    U64 caps = pawnAtt[Us][f] & b.occ[Them] & tgt;
    while (caps) {
      int t = poplsb(caps);
      if (pok(f, t)) {
        if ((1ULL << t) & PRo) {
          list.moves[list.count++] = make_move_enc(f, t, PROMO_Q);
          list.moves[list.count++] = make_move_enc(f, t, PROMO_R);
          list.moves[list.count++] = make_move_enc(f, t, PROMO_B);
          list.moves[list.count++] = make_move_enc(f, t, PROMO_N);
        } else {
          list.moves[list.count++] = make_move_enc(f, t, CAPTURE);
        }
      }
    }
  }
  if (p.ep != -1) {
    U64 ep_p = pawnAtt[Them][p.ep] & b.bb[UsO + PAWN];
    while (ep_p) {
      int f = poplsb(ep_p), cs = p.ep - Up;
      if (nC == 1 && !((1ULL << cs) & tgt) && !((1ULL << p.ep) & tgt))
        continue;
      if (!(attackers_to<Them>(
                b, ksq, (all ^ (1ULL << f) ^ (1ULL << cs)) | (1ULL << p.ep)) &
            (b.occ[Them] ^ (1ULL << cs)))) {
        list.moves[list.count++] = make_move_enc(f, p.ep, EP_CAPTURE);
      }
    }
  }

  U64 kn = b.bb[UsO + KNIGHT] & ~pinned;
  while (kn) {
    int f = poplsb(kn);
    U64 a = knightAtt[f] & tgt;
    while (a) {
      int t = poplsb(a);
      list.moves[list.count++] =
          make_move_enc(f, t, ((1ULL << t) & b.occ[Them]) ? CAPTURE : QUIET);
    }
  }
  for (int pt = BISHOP; pt <= QUEEN; pt++) {
    U64 pce = b.bb[UsO + pt];
    while (pce) {
      int f = poplsb(pce);
      U64 a = (pt == BISHOP ? bishop_attacks(f, all)
                            : (pt == ROOK ? rook_attacks(f, all)
                                          : queen_attacks(f, all))) &
              tgt;
      if (pinned & (1ULL << f))
        a &= LineBB[ksq][f];
      while (a) {
        int t = poplsb(a);
        list.moves[list.count++] =
            make_move_enc(f, t, ((1ULL << t) & b.occ[Them]) ? CAPTURE : QUIET);
      }
    }
  }
  U64 km = kingAtt[ksq] & ~b.occ[Us];
  while (km) {
    int t = poplsb(km);
    if (!attackers_to<Them>(b, t, (all ^ (1ULL << ksq)) | (1ULL << t))) {
      list.moves[list.count++] =
          make_move_enc(ksq, t, ((1ULL << t) & b.occ[Them]) ? CAPTURE : QUIET);
    }
  }

  if (nC == 0 && p.castling) {
    if (Us == WHITE) {
      if ((p.castling & 1) && !(all & 0x60ULL) &&
          !attackers_to<BLACK>(b, 5, all) && !attackers_to<BLACK>(b, 6, all))
        list.moves[list.count++] = make_move_enc(4, 6, CASTLE);
      if ((p.castling & 2) && !(all & 0xEULL) &&
          !attackers_to<BLACK>(b, 3, all) && !attackers_to<BLACK>(b, 2, all))
        list.moves[list.count++] = make_move_enc(4, 2, CASTLE);
    } else {
      if ((p.castling & 4) && !(all & 0x6000000000000000ULL) &&
          !attackers_to<WHITE>(b, 61, all) && !attackers_to<WHITE>(b, 62, all))
        list.moves[list.count++] = make_move_enc(60, 62, CASTLE);
      if ((p.castling & 8) && !(all & 0x0E00000000000000ULL) &&
          !attackers_to<WHITE>(b, 59, all) && !attackers_to<WHITE>(b, 58, all))
        list.moves[list.count++] = make_move_enc(60, 58, CASTLE);
    }
  }
}

void generate_moves(const Position &p, MoveList &list) {
  list.count = 0;
  Position temp =
      p; // Non-const copy for internal use if needed, but generate takes Ref
  if (p.side == WHITE)
    generate<WHITE>(temp, list);
  else
    generate<BLACK>(temp, list);
}

template uint64_t perft<WHITE>(Position &p, int depth);
template uint64_t perft<BLACK>(Position &p, int depth);
