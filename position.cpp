#include "position.h"
#include <cctype>

Position parseFEN(std::string fen) {
  Position p{};
  int r = 7, f = 0;
  size_t i = 0;
  for (; i < fen.size() && fen[i] != ' '; i++) {
    char c = fen[i];
    if (c == '/') {
      r--;
      f = 0;
    } else if (isdigit(c))
      f += c - '0';
    else {
      int sq = r * 8 + f, col = isupper(c) ? WHITE : BLACK, pt;
      switch (tolower(c)) {
      case 'p':
        pt = PAWN;
        break;
      case 'n':
        pt = KNIGHT;
        break;
      case 'b':
        pt = BISHOP;
        break;
      case 'r':
        pt = ROOK;
        break;
      case 'q':
        pt = QUEEN;
        break;
      case 'k':
        pt = KING;
        break;
      }
      p.bb[col][pt] |= 1ULL << sq;
      f++;
    }
  }
  i++;
  p.side = fen[i] == 'w' ? WHITE : BLACK;
  i += 2;
  while (i < fen.size() && fen[i] != ' ') {
    switch (fen[i]) {
    case 'K':
      p.castling |= 1;
      break;
    case 'Q':
      p.castling |= 2;
      break;
    case 'k':
      p.castling |= 4;
      break;
    case 'q':
      p.castling |= 8;
      break;
    }
    i++;
  }
  i++;
  if (fen[i] != '-') {
    p.ep = (fen[i] - 'a') + (fen[i + 1] - '1') * 8;
  } else
    p.ep = -1;
  for (int c = 0; c < 2; c++)
    for (int pt = 0; pt < 6; pt++)
      p.occ[c] |= p.bb[c][pt];
  p.all = p.occ[0] | p.occ[1];
  return p;
}
