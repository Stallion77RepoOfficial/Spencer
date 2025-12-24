#include "spencer.h"
#include <chrono>
#include <iostream>

template <int Us> void run_benchmark(Position &p, int max_depth) {

  for (int d = 1; d <= max_depth; d++) {
    auto start = std::chrono::high_resolution_clock::now();
    uint64_t nodes = perft<Us>(p, d);
    auto end = std::chrono::high_resolution_clock::now();
    std::chrono::duration<double> elapsed = end - start;
    std::cout << "Depth " << d << ": " << nodes << " nodes, " << elapsed.count()
              << " s, " << (nodes / elapsed.count()) << " N/s" << std::endl;
  }
}

int main() {
  init_magics();
  std::cout << "Benchmarking StartPos for 1s..." << std::endl;
  Position p =
      parseFEN("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
  run_benchmark<WHITE>(p, 7);

  return 0;
}
