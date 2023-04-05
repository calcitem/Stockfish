/*
  Stockfish, a UCI chess playing engine derived from Glaurung 2.1
  Copyright (C) 2004-2023 The Stockfish developers (see AUTHORS file)

  Stockfish is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Stockfish is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef MOVEGEN_H_INCLUDED
#define MOVEGEN_H_INCLUDED

#include <algorithm>
#include <array>
#include "types.h"

namespace Stockfish {

class Position;

enum GenType { PLACE, MOVE, REMOVE, LEGAL };

struct ExtMove {
  Move move;
  int value;

  operator Move() const { return move; }
  void operator=(Move m) { move = m; }

  // Inhibit unwanted implicit conversions to Move
  // with an ambiguity that yields to a compile error.
  operator float() const = delete;
};

inline bool operator<(const ExtMove& f, const ExtMove& s) {
  return f.value < s.value;
}


ExtMove* generate(Position& pos, ExtMove* moveList);

/// The MoveList struct is a simple wrapper around generate(). It sometimes comes
/// in handy to use this class instead of the low level generate() function.
template<GenType T>
struct MoveList {

  explicit MoveList(Position& pos) : last(generate(pos, moveList)) {}
  const ExtMove* begin() const { return moveList; }
  const ExtMove* end() const { return last; }
  size_t size() const { return last - moveList; }
  bool contains(Move move) const {
    return std::find(begin(), end(), move) != end();
  }

    static void create();
    //static void shuffle();

    inline static std::array<Square, SQUARE_NB> movePriorityList {
        SQ_16, SQ_18, SQ_20, SQ_22, SQ_24, SQ_26, SQ_28, SQ_30,
        SQ_8,  SQ_10, SQ_12, SQ_14, SQ_17, SQ_19, SQ_21, SQ_23,
        SQ_25, SQ_27, SQ_29, SQ_31, SQ_9,  SQ_11, SQ_13, SQ_15};

    inline static Square adjacentSquares[SQUARE_EXT_NB][MD_NB] = {{SQ_NONE}};
    inline static Bitboard adjacentSquaresBB[SQUARE_EXT_NB] = {0};

private:
  ExtMove moveList[MAX_MOVES], *last;
};

} // namespace Stockfish

#endif // #ifndef MOVEGEN_H_INCLUDED
