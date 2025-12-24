CXX = c++
CXXFLAGS = -O3 -march=native -std=c++23 -Wall

LIB_OBJ = bitboard.o magic.o position.o movegen.o

all: benchmark libspencer.a

libspencer.a: $(LIB_OBJ)
	ar rcs $@ $^

benchmark: benchmark.o libspencer.a
	$(CXX) $(CXXFLAGS) -o $@ benchmark.o -L. -lspencer

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

clean:
	rm -f *.o *.a benchmark spencer_test

.PHONY: all clean
