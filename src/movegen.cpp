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
    // TODO: Sanmill
#include <cassert>

#include "movegen.h"
#include "position.h"

#include "mills.h"

namespace Stockfish {



    /// generate<MOVE> generates all moves.
    /// Returns a pointer to the end of the move moves.
    ExtMove* generate_move(Position& pos, ExtMove* moveList)
    {
        ExtMove* cur = moveList;

        // move piece that location weak first
        for (auto i = SQUARE_NB - 1; i >= 0; i--) {
            const Square from = MoveList<LEGAL>::movePriorityList[i];

            if (!pos.select_piece(from)) {
                continue;
            }

            if (rule.mayFly && pos.piece_on_board_count(pos.side_to_move()) <= rule.flyPieceCount) {
                // piece count < 3 or 4 and allow fly, if is empty point, that's ok,
                // do not need in move list
                for (Square to = SQ_BEGIN; to < SQ_END; ++to) {
                    if (!pos.get_board()[to]) {
                        *cur++ = make_move(from, to);
                    }
                }
            } else {
                for (auto direction = MD_BEGIN; direction < MD_NB; ++direction) {
                    const Square to = MoveList<LEGAL>::adjacentSquares[from][direction];
                    if (to && !pos.get_board()[to]) {
                        *cur++ = make_move(from, to);
                    }
                }
            }
        }

        return cur;
    }

    /// generate<PLACE> generates all places.
    /// Returns a pointer to the end of the move list.
    ExtMove* generate_place(Position& pos, ExtMove* moveList)
    {
        ExtMove* cur = moveList;

        for (auto s : MoveList<LEGAL>::movePriorityList) {
            if (!pos.get_board()[s]) {
                *cur++ = static_cast<Move>(s);
            }
        }

        return cur;
    }

    /// generate<REMOVE> generates all removes.
    /// Returns a pointer to the end of the move moves.
    ExtMove* generate_remove(Position& pos, ExtMove* moveList)
    {
        const Color us = pos.side_to_move();
        const Color them = ~us;

        ExtMove* cur = moveList;

        if (pos.is_stalemate_removal()) {
            for (auto i = SQUARE_NB - 1; i >= 0; i--) {
                Square s = MoveList<LEGAL>::movePriorityList[i];
                if (pos.get_board()[s] & make_piece(them)) {
                    if (pos.is_adjacent_to(s, us) == true) {
                        *cur++ = static_cast<Move>(-s);
                    }
                }
            }

            return cur;
        }

        if (pos.is_all_in_mills(them)) {
#ifndef MADWEASEL_MUEHLE_RULE
            for (auto i = SQUARE_NB - 1; i >= 0; i--) {
                Square s = MoveList<LEGAL>::movePriorityList[i];
                if (pos.get_board()[s] & make_piece(them)) {
                    *cur++ = static_cast<Move>(-s);
                }
            }
#endif
            return cur;
        }

        // not is all in mills
        for (auto i = SQUARE_NB - 1; i >= 0; i--) {
            const Square s = MoveList<LEGAL>::movePriorityList[i];
            if (pos.get_board()[s] & make_piece(them)) {
                if (rule.mayRemoveFromMillsAlways || !pos.potential_mills_count(s, NOBODY)) {
                    *cur++ = static_cast<Move>(-s);
                }
            }
        }

        return cur;
    }

    /// generate<LEGAL> generates all the legal moves in the given position


    ExtMove* generate(Position& pos, ExtMove* moveList)
    {
        ExtMove* cur = moveList;

        switch (pos.get_action()) {
        case Action::select:
        case Action::place:
            if (pos.get_phase() == Phase::placing || pos.get_phase() == Phase::ready) {
                return generate_place(pos, moveList);
            }

            if (pos.get_phase() == Phase::moving) {
                return generate_move(pos, moveList);
            }

            break;

        case Action::remove:
            return generate_remove(pos, moveList);

        case Action::none:
#ifdef FLUTTER_UI
            LOGD("generate(): action = %hu\n", pos.get_action());
#endif
            assert(0);
            break;
        }

        return cur;
    }

    // TODO: Sanmill
        #if 0

    template <GenType Type>
    void MoveList::create()
    {
        Mills::adjacent_squares_init();
    }



    template <GenType Type>
    void MoveList::shuffle()
    {
        Mills::move_priority_list_shuffle();
    }
    #endif


} // namespace Stockfish
