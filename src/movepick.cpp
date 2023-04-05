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

#include <cassert>

#include "bitboard.h"
#include "movepick.h"

namespace Stockfish {

namespace {

  enum Stages {
    MAIN_TT, CAPTURE_INIT, GOOD_CAPTURE, REFUTATION, QUIET_INIT, QUIET, BAD_CAPTURE,
    EVASION_TT, EVASION_INIT, EVASION,
    PROBCUT_TT, PROBCUT_INIT, PROBCUT,
    QSEARCH_TT, QCAPTURE_INIT, QCAPTURE, QCHECK_INIT, QCHECK
  };

  // partial_insertion_sort() sorts moves in descending order up to and including
  // a given limit. The order of moves smaller than the limit is left unspecified.
  void partial_insertion_sort(ExtMove* begin, ExtMove* end, int limit) {

    for (ExtMove *sortedEnd = begin, *p = begin + 1; p < end; ++p)
        if (p->value >= limit)
        {
            ExtMove tmp = *p, *q;
            *p = *++sortedEnd;
            for (q = sortedEnd; q != begin && *(q - 1) < tmp; --q)
                *q = *(q - 1);
            *q = tmp;
        }
  }

} // namespace


/// Constructors of the MovePicker class. As arguments we pass information
/// to help it to return the (presumably) good moves first, to decide which
/// moves to return (in the quiescence search, for instance, we only want to
/// search captures and some checks) and how important good move
/// ordering is at the current node.

/// MovePicker constructor for the main search
MovePicker::MovePicker(const Position& p, Move ttm, Depth d,
                                                             Move cm,
                                                             const Move* killers)
           : pos(p),
             ttMove(ttm), refutations{{killers[0], 0}, {killers[1], 0}, {cm, 0}}, depth(d)
{
  assert(d > 0);

  stage = (MAIN_TT) +
          !(ttm && pos.pseudo_legal(ttm));
  threatenedPieces = 0;
}

/// MovePicker constructor for quiescence search
MovePicker::MovePicker(const Position& p, Move ttm, Depth d,
                                                             Square rs)
           : pos(p), ttMove(ttm), recaptureSquare(rs), depth(d)
{
  assert(d <= 0);

  stage = (QSEARCH_TT) +
          !(   ttm
            && pos.pseudo_legal(ttm));
}

/// MovePicker constructor for ProbCut: we generate captures with SEE greater
/// than or equal to the given threshold.
MovePicker::MovePicker(const Position& p, Move ttm, Value th, ttMove(ttm), threshold(th)
{
   stage = PROBCUT_TT + !(ttm 
                             && pos.pseudo_legal(ttm)
                             && pos.see_ge(ttm, threshold));
}

/// MovePicker::score() assigns a numerical value to each move in a list, used
/// for sorting. Captures are ordered by Most Valuable Victim (MVV), preferring
/// captures with a good history. Quiets moves are ordered using the history tables.
template<GenType Type>
void MovePicker::score() {

    cur = moves;

    int theirMillsCount;
    int ourPieceCount = 0;
    int theirPiecesCount = 0;
    int bannedCount = 0;
    int emptyCount = 0;

    while (cur++->move != MOVE_NONE) {
        Move m = cur->move;

        const Square to = to_sq(m);
        const Square from = from_sq(m);

        // if stat before moving, moving phrase maybe from @-0-@ to 0-@-@, but
        // no mill, so need |from| to judge
        const int ourMillsCount = pos.potential_mills_count(
            to, pos.side_to_move(), from);

#ifndef SORT_MOVE_WITHOUT_HUMAN_KNOWLEDGE
        // TODO(calcitem): rule.mayRemoveMultiple adapt other rules
        if (type_of(m) != MOVETYPE_REMOVE) {
            // all phrase, check if place sq can close mill
            if (ourMillsCount > 0) {
                cur->value += RATING_ONE_MILL * ourMillsCount;
            } else if (pos.get_phase() == Phase::placing) {
                // placing phrase, check if place sq can block their close mill
                theirMillsCount = pos.potential_mills_count(
                    to, ~pos.side_to_move());
                cur->value += RATING_BLOCK_ONE_MILL * theirMillsCount;
            } else if (pos.get_phase() == Phase::moving) {
                // moving phrase, check if place sq can block their close mill
                theirMillsCount = pos.potential_mills_count(
                    to, ~pos.side_to_move());

                if (theirMillsCount) {
                    ourPieceCount = theirPiecesCount = bannedCount =
                        emptyCount = 0;

                    pos.surrounded_pieces_count(to, ourPieceCount,
                                                theirPiecesCount, bannedCount,
                                                emptyCount);

                    if (to % 2 == 0 && theirPiecesCount == 3) {
                        cur->value += RATING_BLOCK_ONE_MILL * theirMillsCount;
                    } else if (to % 2 == 1 && theirPiecesCount == 2 &&
                               rule.hasDiagonalLines) {
                        cur->value += RATING_BLOCK_ONE_MILL * theirMillsCount;
                    }
                }
            }

            // cur->value += bannedCount;  // placing phrase, place nearby ban
            // point

            // If has Diagonal Lines, black 2nd move place star point is as
            // important as close mill (TODO)
            if (rule.hasDiagonalLines &&
                pos.count<ON_BOARD>(BLACK) < 2 && // patch: only when black 2nd
                // move
                Position::is_star_square(static_cast<Square>(m))) {
                cur->value += RATING_STAR_SQUARE;
            }
        } else {
            // Remove
            ourPieceCount = theirPiecesCount = bannedCount = emptyCount = 0;

            pos.surrounded_pieces_count(to, ourPieceCount, theirPiecesCount,
                                        bannedCount, emptyCount);

            if (ourMillsCount > 0) {
                // remove point is in our mill
                // cur->value += RATING_REMOVE_ONE_MILL * ourMillsCount;

                if (theirPiecesCount == 0) {
                    // if remove point nearby has no their piece, preferred.
                    cur->value += 1;
                    if (ourPieceCount > 0) {
                        // if remove point nearby our piece, preferred
                        cur->value += ourPieceCount;
                    }
                }
            }

            // remove point is in their mill
            theirMillsCount = pos.potential_mills_count(to,
                                                        ~pos.side_to_move());
            if (theirMillsCount) {
                if (theirPiecesCount >= 2) {
                    // if nearby their piece, prefer do not remove
                    cur->value -= theirPiecesCount;

                    if (ourPieceCount == 0) {
                        // if nearby has no our piece, more prefer do not remove
                        cur->value -= 1;
                    }
                }
            }

            // prefer remove piece that mobility is strong
            cur->value += emptyCount;
        }
#endif // !SORT_MOVE_WITHOUT_HUMAN_KNOWLEDGE
    }
}

/// MovePicker::select() returns the next move satisfying a predicate function.
/// It never returns the TT move.
template<MovePicker::PickType T, typename Pred>
Move MovePicker::select(Pred filter) {

  while (cur < endMoves)
  {
      if constexpr (T == Best)
          std::swap(*cur, *std::max_element(cur, endMoves));

      if (*cur != ttMove && filter())
          return *cur++;

      cur++;
  }
  return MOVE_NONE;
}

/// MovePicker::next_move() is the most important method of the MovePicker class. It
/// returns a new pseudo-legal move every time it is called until there are no more
/// moves left, picking the move with the highest score from a list of generated moves.
Move MovePicker::next_move(bool skipQuiets) {

    endMoves = generate<LEGAL>(pos, moves);

    score<LEGAL>();
    partial_insertion_sort(moves, endMoves, INT_MIN);

    return *moves;
}

} // namespace Stockfish
