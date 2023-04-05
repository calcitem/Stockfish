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

#ifndef POSITION_H_INCLUDED
#define POSITION_H_INCLUDED

#include <cassert>
#include <deque>
#include <memory> // For std::unique_ptr
#include <string>

#include "bitboard.h"
#include "evaluate.h"
#include "types.h"

#include "nnue/nnue_accumulator.h"

#include "rule.h"

namespace Stockfish {

/// StateInfo struct stores information needed to restore a Position object to
/// its previous state when we retract a move. Whenever a move is made on the
/// board (by calling Position::do_move), a StateInfo object must be passed.

struct StateInfo {
    // TODO: Sanmill
  // Copied when making a move
 Value   material[COLOR_NB];
  int    rule50;
  int    pliesFromNull;

  // Not copied when making a move (will be recomputed anyhow)
  Key        key;
  StateInfo* previous;
  Piece      capturedPiece;
  int        repetition;

  // Used by NNUE
  Eval::NNUE::Accumulator accumulator;
  DirtyPiece dirtyPiece;
};


/// A list to keep track of the position states along the setup moves (from the
/// start position to the position just before the search starts). Needed by
/// 'draw by repetition' detection. Use a std::deque because pointers to
/// elements are not invalidated upon list resizing.
using StateListPtr = std::unique_ptr<std::deque<StateInfo>>;


/// Position class stores information regarding the board representation as
/// pieces, side to move, hash keys, etc. Important methods are
/// do_move() and undo_move(), used by the search to update node info when
/// traversing the search tree.
class Thread;

class Position {
public:
  static void init();

  Position();
  Position(const Position&) = delete;
  Position& operator=(const Position&) = delete;

  // FEN string input/output
  Position& set(const std::string& fenStr, StateInfo* si, Thread* th);
  Position& set(const std::string& code, Color c, StateInfo* si);
  std::string fen() const;

  // Position representation
  Bitboard pieces(PieceType pt) const;
  Bitboard pieces(PieceType pt1, PieceType pt2) const;
  Bitboard pieces(Color c) const;
  Bitboard pieces(Color c, PieceType pt) const;
  Bitboard pieces(Color c, PieceType pt1, PieceType pt2) const;
  Piece piece_on(Square s) const;
  bool empty(Square s) const;
  template<PieceType Pt> int count(Color c) const;
  template<PieceType Pt> int count() const;
  template<PieceType Pt> Square square(Color c) const;

  // Properties of moves
  bool legal(Move m) const;
  bool pseudo_legal(const Move m) const;
  Piece moved_piece(Move m) const;
  Piece captured_piece() const;

  // Doing and undoing moves
  void do_move(Move m, StateInfo& newSt);
  void undo_move(Move m);
  void do_null_move(StateInfo& newSt);
  void undo_null_move();

  // Static Exchange Evaluation
  bool see_ge(Move m, Bitboard& occupied, Value threshold = VALUE_ZERO) const;
  bool see_ge(Move m, Value threshold = VALUE_ZERO) const;

  // Accessing hash keys
  Key key() const;
  Key key_after(Move m) const;

  // Other properties of the position
  Color side_to_move() const;
  int game_ply() const;
  Thread* this_thread() const;
  bool is_draw(int ply) const;
  bool has_game_cycle(int ply) const;
  bool has_repeated() const;
  int rule50_count() const;
  Value material_sum() const;    // TODO: Sanmill
  Value material_diff() const;    // TODO: Sanmill

  // Position consistency check, for debugging
  bool pos_is_ok() const;
  void flip();

  // Used by NNUE
  StateInfo* state() const;

  void put_piece(Piece pc, Square s);
  bool remove_piece(Square s);

private:
  // Initialization helpers (used while setting up a position)
  void set_state() const;

  // Other helpers
  bool move_piece(Square from, Square to);
  template<bool AfterMove>
  Key adjust_key50(Key k) const;

  // Data members
  Piece board[SQUARE_NB];
  Bitboard byTypeBB[PIECE_TYPE_NB];
  Bitboard byColorBB[COLOR_NB];
  //int pieceCount[PIECE_NB]; // TODO: Sanmill
  Thread* thisThread;
  StateInfo* st;
  int gamePly;
  Color sideToMove;

  // Mill
  public:
  Color color_on(Square s) const;
      void construct_key();
    Key revert_key(Square s);
    Key update_key(Square s);
    Key update_key_misc();
	
	Piece *get_board() noexcept;
    [[nodiscard]] Square current_square() const;
    [[nodiscard]] Phase get_phase() const;
    [[nodiscard]] Action get_action() const;
    [[nodiscard]] const char *get_record() const;

    bool reset();
    bool start();
    bool resign(Color loser);
    bool command(const char *cmd);
    void update_score();
    bool check_if_game_is_over();
    void remove_ban_pieces();
    void set_side_to_move(Color c);

    void change_side_to_move();
    [[nodiscard]] Color get_winner() const noexcept;
    void set_gameover(Color w, GameOverReason reason);

    bool is_stalemate_removal();

    void mirror(std::vector<std::string> &moveHistory, bool cmdChange = true);
    void turn(std::vector<std::string> &moveHistory, bool cmdChange = true);
    void rotate(std::vector<std::string> &moveHistory, int degrees,
                bool cmdChange = true);

    void reset_bb();

    static void create_mill_table();
    [[nodiscard]] int mills_count(Square s) const;

    // The number of mills that would be closed by the given move.
    int potential_mills_count(Square to, Color c, Square from = SQ_0);
    bool is_all_in_mills(Color c);

    void surrounded_pieces_count(Square s, int &ourPieceCount,
                                 int &theirPieceCount, int &bannedCount,
                                 int &emptyCount) const;
    [[nodiscard]] bool is_all_surrounded(Color c
#ifdef MADWEASEL_MUEHLE_RULE
                                         ,
                                         Square from = SQ_0, Square to = SQ_0
#endif // MADWEASEL_MUEHLE_RULE
    ) const;

    static void print_board();

    [[nodiscard]] int piece_on_board_count(Color c) const;
    [[nodiscard]] int piece_in_hand_count(Color c) const;

    [[nodiscard]] int piece_to_remove_count(Color c) const;

    [[nodiscard]] int get_mobility_diff() const;
    void updateMobility(MoveType mt, Square s);
    // template <typename Mt> void updateMobility(Square from, Square to);
    int calculate_mobility_diff();

    [[nodiscard]] bool is_three_endgame() const;

    static bool is_star_square(Square s);

    static bool bitboard_is_ok();

    // Other helpers
    bool select_piece(Square s);

    bool put_piece(Square s);

	 int total_mills_count(Color c);
    bool is_board_full_removal_at_placing_phase_end();
    bool is_adjacent_to(Square s, Color c);

	 int pieceInHandCount[COLOR_NB] {0, 9, 9};
    int pieceOnBoardCount[COLOR_NB] {0, 0, 0};
    int pieceToRemoveCount[COLOR_NB] {0, 0, 0};
    bool isNeedStalemateRemoval {false};
    bool isStalemateRemoving {false};
    int mobilityDiff {0};

	Color them {NOCOLOR};
    Color winner;
    GameOverReason gameOverReason {GameOverReason::none};

    Phase phase {Phase::none};
    Action action;

    int score[COLOR_NB] {0};
    int score_draw {0};

    // Relate to Rule
    static Bitboard millTableBB[SQUARE_EXT_NB][LD_NB];

    Square currentSquare;
    int gamesPlayedCount {0};

    static constexpr int RECORD_LEN_MAX = 64;
    char record[RECORD_LEN_MAX] {'\0'};

    Move move {MOVE_NONE};
};

std::ostream& operator<<(std::ostream& os, const Position& pos);

inline Color Position::side_to_move() const {
  return sideToMove;
}

inline Piece Position::piece_on(Square s) const {
  assert(is_ok(s));
  return board[s];
}

inline bool Position::empty(Square s) const {
  return piece_on(s) == NO_PIECE;
}

inline Piece Position::moved_piece(Move m) const {
  return piece_on(from_sq(m));
}

inline Bitboard Position::pieces(PieceType pt = ALL_PIECES) const {
  return byTypeBB[pt];
}

inline Bitboard Position::pieces(PieceType pt1, PieceType pt2) const {
  return pieces(pt1) | pieces(pt2);
}

inline Bitboard Position::pieces(Color c) const {
  return byColorBB[c];
}

inline Bitboard Position::pieces(Color c, PieceType pt) const {
  return pieces(c) & pieces(pt);
}

inline Bitboard Position::pieces(Color c, PieceType pt1, PieceType pt2) const {
  return pieces(c) & (pieces(pt1) | pieces(pt2));
}

template<PieceType Pt> inline int Position::count(Color c) const {
  if (Pt == ON_BOARD) {
      return pieceOnBoardCount[c];
  }

  if (Pt == IN_HAND) {
      return pieceInHandCount[c];
  }

  return 0;
}

template<PieceType Pt> inline int Position::count() const {
  return count<Pt>(WHITE) + count<Pt>(BLACK);
}

template<PieceType Pt> inline Square Position::square(Color c) const {
  assert(count<Pt>(c) == 1);
  return lsb(pieces(c, Pt));
}

inline Key Position::key() const {
  return adjust_key50<false>(st->key);
}

template<bool AfterMove>
inline Key Position::adjust_key50(Key k) const
{
  return st->rule50 < 14 - AfterMove
      ? k : k ^ make_key((st->rule50 - (14 - AfterMove)) / 8);
}

inline Value Position::material_sum() const {
  return st->material[WHITE] + st->material[BLACK];
}

inline Value Position::material_diff() const {
  return st->material[sideToMove] - st->material[~sideToMove];
}

inline int Position::game_ply() const {
  return gamePly;
}

inline int Position::rule50_count() const {
  return st->rule50;
}

inline Piece Position::captured_piece() const {
  return st->capturedPiece;
}

inline Thread* Position::this_thread() const {
  return thisThread;
}

#if 0
inline void Position::put_piece(Piece pc, Square s) {

  board[s] = pc;
  byTypeBB[ALL_PIECES] |= byTypeBB[type_of(pc)] |= s;
  byColorBB[color_of(pc)] |= s;
  //pieceCount[pc]++;
  //pieceCount[make_piece(color_of(pc), ALL_PIECES)]++;
}

inline void Position::remove_piece(Square s) {

  Piece pc = board[s];
  byTypeBB[ALL_PIECES] ^= s;
  byTypeBB[type_of(pc)] ^= s;
  byColorBB[color_of(pc)] ^= s;
  board[s] = NO_PIECE;
  //pieceCount[pc]--;
  //pieceCount[make_piece(color_of(pc), ALL_PIECES)]--;
}

inline void Position::move_piece(Square from, Square to) {

  Piece pc = board[from];
  Bitboard fromTo = from | to;
  byTypeBB[ALL_PIECES] ^= fromTo;
  byTypeBB[type_of(pc)] ^= fromTo;
  byColorBB[color_of(pc)] ^= fromTo;
  board[from] = NO_PIECE;
  board[to] = pc;
}
#endif

inline StateInfo* Position::state() const {

  return st;
}

// Mill

inline Piece *Position::get_board() noexcept
{
    return board;
}

inline Square Position::current_square() const
{
    return currentSquare;
}

inline Phase Position::get_phase() const
{
    return phase;
}

inline Action Position::get_action() const
{
    return action;
}

inline const char *Position::get_record() const
{
    return record;
}

inline int Position::piece_on_board_count(Color c) const
{
    return pieceOnBoardCount[c];
}

inline int Position::piece_in_hand_count(Color c) const
{
    return pieceInHandCount[c];
}

inline int Position::piece_to_remove_count(Color c) const
{
    return pieceToRemoveCount[c];
}

inline int Position::get_mobility_diff() const
{
    return mobilityDiff;
}

inline bool Position::is_three_endgame() const
{
    if (get_phase() == Phase::placing) {
        return false;
    }

    return pieceOnBoardCount[WHITE] == 3 || pieceOnBoardCount[BLACK] == 3;
}

} // namespace Stockfish

#endif // #ifndef POSITION_H_INCLUDED
