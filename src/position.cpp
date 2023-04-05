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

#include <algorithm>
#include <cassert>
#include <cstddef> // For offsetof()
#include <cstring> // For std::memset, std::memcmp
#include <iomanip>
#include <sstream>
#include <string_view>

#include "bitboard.h"
#include "misc.h"
#include "movegen.h"
#include "position.h"
#include "thread.h"
#include "tt.h"
#include "uci.h"

#include "mills.h"
#include "option.h"

using std::string;

namespace Stockfish {

namespace Zobrist {
  constexpr int KEY_MISC_BIT = 2;
  Key psq[PIECE_NB][SQUARE_EXT_NB];    // TODO: Sanmill
  Key side;
}

namespace {

string PieceToChar(Piece p)
{
    if (p == NO_PIECE) {
        return "*";
    }

    if (p == BAN_PIECE) {
        return "X";
    }

    if (W_PIECE <= p && p <= W_PIECE_12) {
        return "O";
    }

    if (B_PIECE <= p && p <= B_PIECE_12) {
        return "@";
    }

    return "*";
}

Piece CharToPiece(char ch) noexcept
{
    if (ch == '*') {
        return NO_PIECE;
    }

    if (ch == 'O') {
        return W_PIECE;
    }

    if (ch == '@') {
        return B_PIECE;
    }

    if (ch == 'X') {
        return BAN_PIECE;
    }

    return NO_PIECE;
}

constexpr Piece Pieces[] = {NO_PIECE, W_PIECE, B_PIECE,
                                    BAN_PIECE};
} // namespace


/// operator<<(Position) returns an ASCII representation of the position

std::ostream& operator<<(std::ostream& os, const Position& pos) {

    /*
        X --- X --- X
        |\    |    /|
        | X - X - X |
        | |\  |  /| |
        | | X-X-X | |
        X-X-X   X-X-X
        | | X-X-X | |
        | |/  |  \| |
        | X - X - X |
        |/    |    \|
        X --- X --- X
    */

    /*
        31 ----- 24 ----- 25
        | \       |      / |
        |  23 -- 16 -- 17  |
        |  | \    |   / |  |
        |  |  15 08 09  |  |
        30-22-14    10-18-26
        |  |  13 12 11  |  |
        |  | /    |   \ |  |
        |  21 -- 20 -- 19  |
        | /       |     \  |
        29 ----- 28 ----- 27
    */

#define P(s) PieceToChar(pos.piece_on(Square(s)))

    if (rule.hasDiagonalLines) {
        os << "\n";
        os << P(31) << " --- " << P(24) << " --- " << P(25) << "\n";
        os << "|\\    |    /|\n";
        os << "| " << P(23) << " - " << P(16) << " - " << P(17) << " |\n";
        os << "| |\\  |  /| |\n";
        os << "| | " << P(15) << "-" << P(8) << "-" << P(9) << " | |\n";
        os << P(30) << "-" << P(22) << "-" << P(14) << "   " << P(10) << "-"
           << P(18) << "-" << P(26) << "\n";
        os << "| | " << P(13) << "-" << P(12) << "-" << P(11) << " | |\n";
        os << "| |/  |  \\| |\n";
        os << "| " << P(21) << " - " << P(20) << " - " << P(19) << " |\n";
        os << "|/    |    \\|\n";
        os << P(29) << " --- " << P(28) << " --- " << P(27) << "\n";
    } else {
        os << "\n";
        os << P(31) << " --- " << P(24) << " --- " << P(25) << "\n";
        os << "|     |     |\n";
        os << "| " << P(23) << " - " << P(16) << " - " << P(17) << " |\n";
        os << "| |   |   | |\n";
        os << "| | " << P(15) << "-" << P(8) << "-" << P(9) << " | |\n";
        os << P(30) << "-" << P(22) << "-" << P(14) << "   " << P(10) << "-"
           << P(18) << "-" << P(26) << "\n";
        os << "| | " << P(13) << "-" << P(12) << "-" << P(11) << " | |\n";
        os << "| |   |   | |\n";
        os << "| " << P(21) << " - " << P(20) << " - " << P(19) << " |\n";
        os << "|     |     |\n";
        os << P(29) << " --- " << P(28) << " --- " << P(27) << "\n";
    }

#undef P

    const auto fill = os.fill();
    const auto flags = os.flags();

    os << "\nFen: " << pos.fen() << "\nKey: " << std::hex << std::uppercase
       << std::setfill('0') << std::setw(16) << pos.key() << std::endl;

    os.flags(flags);
    os.fill(fill);

    return os;
}

// TODO: Sanmill
Position::Position()
{
    reset();
}

// Marcel van Kervinck's cuckoo algorithm for fast detection of "upcoming repetition"
// situations. Description of the algorithm in the following paper:
// http://web.archive.org/web/20201107002606/https://marcelk.net/2013-04-06/paper/upcoming-rep-v2.pdf

// First and second hash functions for indexing the cuckoo tables
inline int H1(Key h) { return h & 0x1fff; }
inline int H2(Key h) { return (h >> 16) & 0x1fff; }

// Cuckoo tables with Zobrist hashes of valid reversible moves, and the moves themselves
Key cuckoo[8192];
Move cuckooMove[8192];


/// Position::init() initializes at startup the various arrays used to compute hash keys

void Position::init() {

    PRNG rng(1070372);

    for (Piece pc : Pieces) // TODO: Sanmill
        for (Square s = SQ_BEGIN; s < SQ_END; ++s)
        Zobrist::psq[pc][s] = rng.rand<Key>() << Zobrist::KEY_MISC_BIT >>
                                  Zobrist::KEY_MISC_BIT;

    Zobrist::side = rng.rand<Key>() << Zobrist::KEY_MISC_BIT >>
                    Zobrist::KEY_MISC_BIT;
}


/// Position::set() initializes the position object with the given FEN string.
/// This function is not very robust - make sure that input FENs are correct,
/// this is assumed to be the responsibility of the GUI.
    // TODO: Sanmill
Position& Position::set(const string& fenStr, StateInfo* si, Thread* th) {
 /*
       A FEN string defines a particular position using only the ASCII character
       set.

       A FEN string contains six fields separated by a space. The fields are:

       1) Piece placement. Each rank is described, starting
          with rank 1 and ending with rank 8. Within each rank, the contents of
       each square are described from file A through file C. Following the
       Standard Algebraic Notation (SAN), each piece is identified by a single
       letter taken from the standard English names. White pieces are designated
       using "O" whilst Black uses "@". Blank uses "*". Banned uses "X". noted
       using digits 1 through 8 (the number of blank squares), and "/" separates
       ranks.

       2) Active color. "w" means white moves next, "b" means black.

       3) Phrase.

       4) Action.

       5) White on board/White in hand/Black on board/Black in hand/need to
       remove

       6) Halfmove clock. This is the number of halfmoves since the last
          capture. This is used to determine if a draw can be claimed under the
          N-move rule.

       7) Fullmove number. The number of the full move. It starts at 1, and is
          incremented after White's move.
    */



   st = si;

    unsigned char token = '\0';
    Square sq = SQ_A1;
    std::istringstream ss(fenStr);

    ss >> std::noskipws;

    // 1. Piece placement
    while ((ss >> token) && !isspace(token)) {
        if (token == 'O' || token == '@' || token == 'X') {
            put_piece(CharToPiece(token), sq);
            ++sq;
        }
        if (token == '*') {
            ++sq;
        }
    }

    // 2. Active color
    ss >> token;
    sideToMove = (token == 'w' ? WHITE : BLACK);
    them = ~sideToMove; // Note: Stockfish do not need to set them

    // 3. Phrase
    ss >> token;
    ss >> token;

    switch (token) {
    case 'r':
        phase = Phase::ready;
        break;
    case 'p':
        phase = Phase::placing;
        break;
    case 'm':
        phase = Phase::moving;
        break;
    case 'o':
        phase = Phase::gameOver;
        break;
    default:
        phase = Phase::none;
    }

    // 4. Action
    ss >> token;
    ss >> token;

    switch (token) {
    case 'p':
        action = Action::place;
        break;
    case 's':
        action = Action::select;
        break;
    case 'r':
        action = Action::remove;
        break;
    default:
        action = Action::none;
    }

    // 5. White on board / White in hand / Black on board / Black in hand /
    // White need to remove / Black need to remove
    ss >> std::skipws >> pieceOnBoardCount[WHITE] >> pieceInHandCount[WHITE] >>
        pieceOnBoardCount[BLACK] >> pieceInHandCount[BLACK] >>
        pieceToRemoveCount[WHITE] >> pieceToRemoveCount[BLACK];

    // 6-7. Halfmove clock and fullmove number
    ss >> std::skipws >> st->rule50 >> gamePly;

    // Convert from fullmove starting from 1 to gamePly starting from 0,
    // handle also common incorrect FEN with fullmove = 0.
    gamePly = std::max(2 * (gamePly - 1), 0) + (sideToMove == BLACK);

    // For Mill only
    check_if_game_is_over();
#if 0
    // It doesn't work
    if (pieceToRemoveCount[sideToMove] == 1) {
        action = Action::remove;
        isStalemateRemoving = true;
    }
#endif

    thisThread = th;
    set_state();

    return *this;
}

/// Position::set_state() computes the hash keys of the position, and other
/// data that once computed is updated incrementally as moves are made.
/// The function is only used when a new position is set up, and to verify
/// the correctness of the StateInfo data when running in debug mode.

void Position::set_state() const {

// TODO: Sanmill
  st->key = 0;
  st->material[WHITE] = st->material[BLACK] = VALUE_ZERO;

  #if 0
  for (Bitboard b = pieces(); b; )
  {
      Square s = pop_lsb(b);
      Piece pc = piece_on(s);
      st->key ^= Zobrist::psq[pc][s];
// TODO: Sanmill
      st->material[color_of(pc)] += PieceValue;
  }
  #endif

  if (sideToMove == BLACK)
      st->key ^= Zobrist::side;
}

/// Position::fen() returns a FEN representation of the position.

string Position::fen() const {
    // TODO: Sanmill
    std::ostringstream ss;

    // Piece placement data
    for (File f = FILE_A; f <= FILE_C; ++f) {
        for (Rank r = RANK_1; r <= RANK_8; ++r) {
            ss << PieceToChar(piece_on(make_square(f, r)));
        }

        if (f == FILE_C) {
            ss << " ";
        } else {
            ss << "/";
        }
    }

    // Active color
    ss << (sideToMove == WHITE ? "w" : "b");

    ss << " ";

    // Phrase
    switch (phase) {
    case Phase::none:
        ss << "n";
        break;
    case Phase::ready:
        ss << "r";
        break;
    case Phase::placing:
        ss << "p";
        break;
    case Phase::moving:
        ss << "m";
        break;
    case Phase::gameOver:
        ss << "o";
        break;
    }

    ss << " ";

    // Action
    switch (action) {
    case Action::place:
        ss << "p";
        break;
    case Action::select:
        ss << "s";
        break;
    case Action::remove:
        ss << "r";
        break;
    case Action::none:
        ss << "?";
        break;
    }

    ss << " ";

    ss << pieceOnBoardCount[WHITE] << " " << pieceInHandCount[WHITE] << " "
       << pieceOnBoardCount[BLACK] << " " << pieceInHandCount[BLACK] << " "
       << pieceToRemoveCount[WHITE] << " " << pieceToRemoveCount[BLACK] << " ";

    ss << st->rule50 << " " << 1 + (gamePly - (sideToMove == BLACK)) / 2;

    return ss.str();
}


/// Position::legal() tests whether a pseudo-legal move is legal

bool Position::legal(Move m) const {

    assert(is_ok(m));

    const Color us = sideToMove;
    const Square from = from_sq(m);
    const Square to = to_sq(m);

    if (from == to) {
        return false;
    }

    if (phase == Phase::moving && type_of(move) != MOVETYPE_REMOVE) {
        if (color_of(moved_piece(m)) != us) {
            return false;
        }
    }

    return true;  // TODO: Sanmill
}


/// Position::pseudo_legal() takes a random move and tests whether the move is
/// pseudo legal. It is used to validate moves from TT that can be corrupted
/// due to SMP concurrent access or hash position key aliasing.

bool Position::pseudo_legal(const Move m) const {
    // TODO: Sanmill
  Color us = sideToMove;
  Square from = from_sq(m);
  Square to = to_sq(m);
  Piece pc = moved_piece(m);

  // If the 'from' square is not occupied by a piece belonging to the side to
  // move, the move is obviously not legal.
  if (pc == NO_PIECE || color_of(pc) != us)
      return false;

  // The destination square cannot be occupied by a friendly piece
  if (pieces(us) & to)
      return false;

  return true;
}


/// Position::do_move() makes a move, and saves all information necessary
/// to a StateInfo object. The move is assumed to be legal. Pseudo-legal
/// moves should be filtered out before this function is called.

void Position::do_move(Move m, StateInfo& newSt) {
// TODO: Sanmill
  assert(is_ok(m));
  assert(&newSt != st);

  thisThread->nodes.fetch_add(1, std::memory_order_relaxed);
  Key k = st->key ^ Zobrist::side;

  // Copy some fields of the old state to our new StateInfo object except the
  // ones which are going to be recalculated from scratch anyway and then switch
  // our state pointer to point to the new (ready to be updated) state.
  std::memcpy(&newSt, st, offsetof(StateInfo, key));
  newSt.previous = st;
  st = &newSt;

  // Increment ply counters. In particular, rule50 will be reset to zero later on
  // in case of a capture or a pawn move.
  ++gamePly;
  ++st->rule50;
  ++st->pliesFromNull;

  // Used by NNUE
  st->accumulator.computed[WHITE] = false;
  st->accumulator.computed[BLACK] = false;
  auto& dp = st->dirtyPiece;
  dp.dirty_num = 1;

  Color us = sideToMove;
  Color them = ~us;
  Square from = from_sq(m);
  Square to = to_sq(m);
  Piece pc = piece_on(from);

  auto captured = piece_on(to);

  //assert(color_of(pc) == us);

  if (type_of(m) == MOVETYPE_REMOVE)
  {
      Square capsq = to;
    // TODO: Sanmill

      st->material[them] -= PieceValue;

      if (Eval::useNNUE)
      {
          dp.dirty_num = 2;  // 1 piece moved, 1 piece captured
          dp.piece[1] = captured;
          dp.from[1] = capsq;
          dp.to[1] = SQ_NONE;
      }

      // Update board and piece lists
      remove_piece(capsq);

      // Reset rule 50 counter
      st->rule50 = 0;
  }

  // Update hash key
  k ^= Zobrist::psq[pc][from] ^ Zobrist::psq[pc][to];

  // Set capture piece
  st->capturedPiece = captured;

  // Update the key with the final value
  st->key = k;

  sideToMove = ~sideToMove;

  // Calculate the repetition info. It is the ply distance from the previous
  // occurrence of the same position, negative in the 3-fold case, or zero
  // if the position was not repeated.
  st->repetition = 0;
  int end = std::min(st->rule50, st->pliesFromNull);
  if (end >= 4)
  {
      StateInfo* stp = st->previous->previous;
      for (int i = 4; i <= end; i += 2)
      {
          stp = stp->previous->previous;
          if (stp->key == st->key)
          {
              st->repetition = stp->repetition ? -i : i;
              break;
          }
      }
  }

  assert(pos_is_ok());
}


/// Position::undo_move() unmakes a move. When it returns, the position should
/// be restored to exactly the same state as before the move was made.

void Position::undo_move(Move m) {

  assert(is_ok(m));

  sideToMove = ~sideToMove;

  Color us = sideToMove;
  Square from = from_sq(m);
  Square to = to_sq(m);
  Piece pc = piece_on(to);

      move_piece(to, from); // Put the piece back at the source square

      if (st->capturedPiece)
      {
          Square capsq = to;

          put_piece(st->capturedPiece, capsq); // Restore the captured piece
      }


  // Finally point our state pointer back to the previous state
  st = st->previous;
  --gamePly;

  assert(pos_is_ok());
}


/// Position::do_null_move() is used to do a "null move": it flips
/// the side to move without executing any move on the board.

void Position::do_null_move(StateInfo& newSt) {

  assert(&newSt != st);

  std::memcpy(&newSt, st, offsetof(StateInfo, accumulator));

  newSt.previous = st;
  st = &newSt;

  st->dirtyPiece.dirty_num = 0;
  st->dirtyPiece.piece[0] = NO_PIECE; // Avoid checks in UpdateAccumulator()
  st->accumulator.computed[WHITE] = false;
  st->accumulator.computed[BLACK] = false;

  st->key ^= Zobrist::side;
  ++st->rule50;
  prefetch(TT.first_entry(key()));

  st->pliesFromNull = 0;

  sideToMove = ~sideToMove;

  st->repetition = 0;

  assert(pos_is_ok());
}


/// Position::undo_null_move() must be used to undo a "null move"

void Position::undo_null_move() {

  st = st->previous;
  sideToMove = ~sideToMove;
}


/// Position::key_after() computes the new hash key after the given move. Needed
/// for speculative prefetch.

Key Position::key_after(Move m) const {

  Square from = from_sq(m);
  Square to = to_sq(m);
  Piece pc = piece_on(from);
  Piece captured = piece_on(to);
  Key k = st->key ^ Zobrist::side;

  if (captured)
      k ^= Zobrist::psq[captured][to];

  k ^= Zobrist::psq[pc][to] ^ Zobrist::psq[pc][from];

  return (captured)
      ? k : adjust_key50<true>(k);
}


/// Position::see_ge (Static Exchange Evaluation Greater or Equal) tests if the
/// SEE value of move is greater or equal to the given threshold. We'll use an
/// algorithm similar to alpha-beta pruning with a null window.

bool Position::see_ge(Move m, Bitboard& occupied, Value threshold) const {

  assert(is_ok(m));

  Square from = from_sq(m), to = to_sq(m);

  int swap = PieceValue - threshold;
  if (swap < 0)
      return false;

  swap = PieceValue - swap;
  if (swap <= 0)
      return true;

  assert(color_of(piece_on(from)) == sideToMove);
  occupied = pieces() ^ from ^ to; // xoring to is important for pinned piece logic
  Color stm = sideToMove;
  Bitboard stmAttackers, bb;
  int res = 1;

  // TODO: Sanmill

  return bool(res);
}

bool Position::see_ge(Move m, Value threshold) const {
    Bitboard occupied;
    return see_ge(m, occupied, threshold);
}


/// Position::is_draw() tests whether the position is drawn by 50-move rule
/// or by repetition. It does not detect stalemates.

bool Position::is_draw(int ply) const {

  if (st->rule50 > 99) // TODO: Sanmill: (MoveList<LEGAL>(*this).size())
      return true;

  // Return a draw score if a position repeats once earlier but strictly
  // after the root, or repeats twice before or at the root.
  return st->repetition && st->repetition < ply;
}


// Position::has_repeated() tests whether there has been at least one repetition
// of positions since the last capture or pawn move.

bool Position::has_repeated() const {

    StateInfo* stc = st;
    int end = std::min(st->rule50, st->pliesFromNull);
    while (end-- >= 4)
    {
        if (stc->repetition)
            return true;

        stc = stc->previous;
    }
    return false;
}


/// Position::has_game_cycle() tests if the position has a move which draws by repetition,
/// or an earlier position has a move that directly reaches the current position.

bool Position::has_game_cycle(int ply) const {

  int j;

  int end = std::min(st->rule50, st->pliesFromNull);

  if (end < 3)
    return false;

  Key originalKey = st->key;
  StateInfo* stp = st->previous;

  for (int i = 3; i <= end; i += 2)
  {
      stp = stp->previous->previous;
        // TODO: Sanmill


              // For repetitions before or at the root, require one more
              if (stp->repetition)
                  return true;


  }
  return false;
}


/// Position::flip() flips position with the white and black sides reversed. This
/// is only useful for debugging e.g. for finding evaluation symmetry bugs.

void Position::flip() {

  string f, token;
  std::stringstream ss(fen());

  for (Rank r = RANK_8; r >= RANK_1; --r) // Piece placement
  {
      std::getline(ss, token, r > RANK_1 ? '/' : ' ');
      f.insert(0, token + (f.empty() ? " " : "/"));
  }

  ss >> token; // Active color
  f += (token == "w" ? "B " : "W "); // Will be lowercased later

  ss >> token; // Castling availability
  f += token + " ";

  std::transform(f.begin(), f.end(), f.begin(),
                 [](char c) { return char(islower(c) ? toupper(c) : tolower(c)); });

  ss >> token; // En passant square
  f += (token == "-" ? token : token.replace(1, 1, token[1] == '3' ? "6" : "3"));    // TODO: Sanmill

  std::getline(ss, token); // Half and full moves
  f += token;

  set(f, st, this_thread());

  assert(pos_is_ok());
}


/// Position::pos_is_ok() performs some consistency checks for the
/// position object and raises an asserts if something wrong is detected.
/// This is meant to be helpful when debugging.

bool Position::pos_is_ok() const {

  constexpr bool Fast = true; // Quick (default) or full check?

  if (   (sideToMove != WHITE && sideToMove != BLACK))
      assert(0 && "pos_is_ok: Default");

  if (Fast)
      return true;

  if (   (pieces(WHITE) & pieces(BLACK))
      || (pieces(WHITE) | pieces(BLACK)) != pieces()
      || popcount(pieces(WHITE)) > 16
      || popcount(pieces(BLACK)) > 16)
      assert(0 && "pos_is_ok: Bitboards");

  #if 0
  for (Piece pc : Pieces)
      if (   pieceCount[pc] != popcount(pieces(color_of(pc), type_of(pc)))
          || pieceCount[pc] != std::count(board, board + SQUARE_NB, pc))
          assert(0 && "pos_is_ok: Pieces");
  #endif

  return true;
}

// Mill

/// Mill Game

bool Position::reset()
{
    gamePly = 0;
    //st->rule50 = 0;

    phase = Phase::ready;
    set_side_to_move(WHITE);
    action = Action::place;

    winner = NOBODY;
    gameOverReason = GameOverReason::none;

    memset(board, 0, sizeof(board));
    memset(byTypeBB, 0, sizeof(byTypeBB));
    memset(byColorBB, 0, sizeof(byColorBB));

    //st->key = 0;

    pieceOnBoardCount[WHITE] = pieceOnBoardCount[BLACK] = 0;
    pieceInHandCount[WHITE] = pieceInHandCount[BLACK] = rule.pieceCount;
    pieceToRemoveCount[WHITE] = pieceToRemoveCount[BLACK] = 0;

    isNeedStalemateRemoval = false;
    isStalemateRemoving = false;

    mobilityDiff = 0;

    //MoveList<LEGAL>::create();
    create_mill_table();
    currentSquare = SQ_0;

#ifdef ENDGAME_LEARNING
    if (gameOptions.isEndgameLearningEnabled() && gamesPlayedCount > 0 &&
        gamesPlayedCount % SAVE_ENDGAME_EVERY_N_GAMES == 0) {
        Thread::saveEndgameHashMapToFile();
    }
#endif /* ENDGAME_LEARNING */

    int r;
    for (r = 0; r < N_RULES; r++) {
        if (strcmp(rule.name, RULES[r].name) == 0)
            break;
    }

    if (snprintf(record, RECORD_LEN_MAX, "r%1d s%03u t%02d", r + 1,
                 rule.nMoveRule, 0) > 0) {
        return true;
    }

    record[0] = '\0';

    return false;
}

bool Position::start()
{
    gameOverReason = GameOverReason::none;

    switch (phase) {
    case Phase::placing:
    case Phase::moving:
        return false;
    case Phase::gameOver:
        reset();
        [[fallthrough]];
    case Phase::ready:
        phase = Phase::placing;
        return true;
    case Phase::none:
        return false;
    }

    return false;
}

bool Position::put_piece(Square s)
{
    const Color us = sideToMove;

    if (phase == Phase::gameOver || action != Action::place ||
        !(SQ_BEGIN <= s && s < SQ_END) || board[s]) {
        return false;
    }

    isNeedStalemateRemoval = false;

    if (phase == Phase::ready) {
        start();
    }

    if (phase == Phase::placing) {
        const auto piece = static_cast<Piece>((0x01 | make_piece(sideToMove)) +
                                              rule.pieceCount -
                                              pieceInHandCount[us]);
        pieceInHandCount[us]--;
        pieceOnBoardCount[us]++;

        const Piece pc = board[s] = piece;
        byTypeBB[ALL_PIECES] |= byTypeBB[type_of(pc)] |= s;
        byColorBB[color_of(pc)] |= s; // TODO(calcitem): Put ban?

        update_key(s);

        updateMobility(MOVETYPE_PLACE, s);

        currentSquare = s;

#ifdef MADWEASEL_MUEHLE_RULE
        if (pieceInHandCount[WHITE] == 0 && pieceInHandCount[BLACK] == 0 &&
            is_all_surrounded(~sideToMove, SQ_0, s)) {
            set_gameover(sideToMove, GameOverReason::loseNoWay);
            // change_side_to_move();
            return true;
        }
#endif

        const int n = mills_count(currentSquare);

        if (n == 0
#ifdef MADWEASEL_MUEHLE_RULE
            || is_all_in_mills(them)
#endif
        ) {
            if (pieceInHandCount[WHITE] < 0 || pieceInHandCount[BLACK] < 0) {
                return false;
            }

            if (pieceInHandCount[WHITE] == 0 && pieceInHandCount[BLACK] == 0) {
                if (check_if_game_is_over()) {
                    return true;
                }

                if (pieceToRemoveCount[sideToMove] > 0) {
                    action = Action::remove;
                    update_key_misc();
                } else {
                    phase = Phase::moving;
                    action = Action::select;

                    if (rule.hasBannedLocations) {
                        remove_ban_pieces();
                    }

                    if (!rule.isDefenderMoveFirst) {
                        change_side_to_move();
                    }

                    if (check_if_game_is_over()) {
                        return true;
                    }
                }
            } else {
                change_side_to_move();
            }
        } else {
            pieceToRemoveCount[sideToMove] = rule.mayRemoveMultiple ? n : 1;
            update_key_misc();

            if (rule.mayOnlyRemoveUnplacedPieceInPlacingPhase) {
                pieceInHandCount[them] -= 1; // Or pieceToRemoveCount?;

                if (pieceInHandCount[them] < 0) {
                    pieceInHandCount[them] = 0;
                }

                if (pieceInHandCount[WHITE] < 0 ||
                    pieceInHandCount[BLACK] < 0) {
                    return false;
                }

                if (pieceInHandCount[WHITE] == 0 &&
                    pieceInHandCount[BLACK] == 0) {
                    if (check_if_game_is_over()) {
                        return true;
                    }

                    phase = Phase::moving;
                    action = Action::select;

                    if (rule.isDefenderMoveFirst) {
                        change_side_to_move();
                    }

                    if (check_if_game_is_over()) {
                        return true;
                    }
                }
            } else {
                action = Action::remove;
            }
        }

    } else if (phase == Phase::moving) {
#ifdef MADWEASEL_MUEHLE_RULE
        if (is_all_surrounded(~sideToMove, currentSquare, s)) {
            set_gameover(sideToMove, GameOverReason::loseNoWay);
        }
#else
        if (check_if_game_is_over()) {
            return true;
        }
#endif // MADWEASEL_MUEHLE_RULE

        // If illegal
        if (pieceOnBoardCount[sideToMove] > rule.flyPieceCount ||
            !rule.mayFly) {
            if ((square_bb(s) &
                 MoveList<LEGAL>::adjacentSquaresBB[currentSquare]) == 0) {
                return false;
            }
        }

        
        st->rule50++;        

        const Piece pc = board[currentSquare];

        CLEAR_BIT(byTypeBB[ALL_PIECES], currentSquare);
        CLEAR_BIT(byTypeBB[type_of(pc)], currentSquare);
        CLEAR_BIT(byColorBB[color_of(pc)], currentSquare);

        updateMobility(MOVETYPE_REMOVE, currentSquare);

        SET_BIT(byTypeBB[ALL_PIECES], s);
        SET_BIT(byTypeBB[type_of(pc)], s);
        SET_BIT(byColorBB[color_of(pc)], s);

        updateMobility(MOVETYPE_PLACE, s);

        board[s] = pc;
        update_key(s);
        revert_key(currentSquare);

        board[currentSquare] = NO_PIECE;

        currentSquare = s;

        const int n = mills_count(currentSquare);

        if (n == 0
#ifdef MADWEASEL_MUEHLE_RULE
            || is_all_in_mills(them)
#endif
        ) {
            action = Action::select;
            change_side_to_move();

            if (check_if_game_is_over()) {
                return true;
            }

            if (pieceToRemoveCount[sideToMove] == 1) {
                update_key_misc();
                action = Action::remove;
                isNeedStalemateRemoval = true;
            }
        } else {
            pieceToRemoveCount[sideToMove] = rule.mayRemoveMultiple ? n : 1;
            update_key_misc();
            action = Action::remove;
        }
    } else {
        assert(0);
    }

    return true;
}

// TODO: Sanmill
#if 0
bool Position::remove_piece(Square s)
{
    if (phase == Phase::ready || phase == Phase::gameOver)
        return false;

    if (action != Action::remove)
        return false;

    if (pieceToRemoveCount[sideToMove] <= 0)
        return false;

    // if piece is not their
    if (!(make_piece(~side_to_move()) & board[s]))
        return false;

    if (is_stalemate_removal()) {
        if (is_adjacent_to(s, sideToMove) == false) {
            return false;
        }
    } else if (!rule.mayRemoveFromMillsAlways &&
               potential_mills_count(s, NOBODY)
#ifndef MADWEASEL_MUEHLE_RULE
               && !is_all_in_mills(~sideToMove)
#endif
    ) {
        return false;
    }

    revert_key(s);

    Piece pc = board[s];

    CLEAR_BIT(byTypeBB[type_of(pc)],
              s); // TODO(calcitem): rule.hasBannedLocations and placing need?
    CLEAR_BIT(byColorBB[color_of(pc)], s);

    updateMobility(MOVETYPE_REMOVE, s);

    if (rule.hasBannedLocations && phase == Phase::placing) {
        // Remove and put ban
        pc = board[s] = BAN_PIECE;
        update_key(s);
        SET_BIT(byTypeBB[type_of(pc)], s);
    } else {
        // Remove only
        CLEAR_BIT(byTypeBB[ALL_PIECES], s);
        board[s] = NO_PIECE;
    }

    
    st.rule50 = 0; // TODO(calcitem): Need to move out?
    
    pieceOnBoardCount[them]--;

    if (pieceOnBoardCount[them] + pieceInHandCount[them] <
        rule.piecesAtLeastCount) {
        set_gameover(sideToMove, GameOverReason::loseLessThanThree);
        return true;
    }

    currentSquare = SQ_0;

    pieceToRemoveCount[sideToMove]--;
    update_key_misc();

    if (pieceToRemoveCount[sideToMove] > 0) {
        return true;
    }

    if (isStalemateRemoving) {
        isStalemateRemoving = false;
    } else {
        change_side_to_move();
    }

    if (pieceToRemoveCount[sideToMove] > 0) {
        return true;
    }

    if (phase == Phase::placing) {
        if (pieceInHandCount[WHITE] == 0 && pieceInHandCount[BLACK] == 0) {
            phase = Phase::moving;
            action = Action::select;

            if (rule.hasBannedLocations) {
                remove_ban_pieces();
            }

            if (rule.isDefenderMoveFirst) {
                set_side_to_move(BLACK);
                goto check;
            } else {
                set_side_to_move(WHITE);
            }
        } else {
            action = Action::place;
        }
    } else {
        action = Action::select;
    }

check:
    if (check_if_game_is_over()) {
        return true;
    }

    return true;
}
#endif

bool Position::select_piece(Square s)
{
    if (phase != Phase::moving)
        return false;

    if (action != Action::select && action != Action::place)
        return false;

    if (board[s] & make_piece(sideToMove)) {
        currentSquare = s;
        action = Action::place;

        return true;
    }

    return false;
}

bool Position::resign(Color loser)
{
    if (phase == Phase::ready || phase == Phase::gameOver ||
        phase == Phase::none) {
        return false;
    }

    set_gameover(~loser, GameOverReason::loseResign);

    snprintf(record, RECORD_LEN_MAX, loseReasonResignStr, loser);

    return true;
}

Color Position::get_winner() const noexcept
{
    return winner;
}

void Position::set_gameover(Color w, GameOverReason reason)
{
    phase = Phase::gameOver;
    gameOverReason = reason;
    winner = w;

    update_score();
}

void Position::update_score()
{
    if (phase == Phase::gameOver) {
        if (winner == DRAW) {
            score_draw++;
            return;
        }

        score[winner]++;
    }
}

bool Position::check_if_game_is_over()
{
#ifdef RULE_50
    // TODO: Sanmill
    #if 0
    if (rule.nMoveRule > 0 && posKeyHistory.size() >= rule.nMoveRule) {
        set_gameover(DRAW, GameOverReason::drawRule50);
        return true;
    }

    if (rule.endgameNMoveRule < rule.nMoveRule && is_three_endgame() &&
        posKeyHistory.size() >= rule.endgameNMoveRule) {
        set_gameover(DRAW, GameOverReason::drawEndgameRule50);
        return true;
    }
    #endif
#endif // RULE_50

    if (rule.pieceCount == 12 &&
        (pieceOnBoardCount[WHITE] + pieceOnBoardCount[BLACK] >= SQUARE_NB)) {
        // TODO: BoardFullAction: Support other actions
        switch (rule.boardFullAction) {
        case BoardFullAction::firstPlayerLose:
            set_gameover(BLACK, GameOverReason::loseBoardIsFull);
            return true;
        case BoardFullAction::firstAndSecondPlayerRemovePiece:
            pieceToRemoveCount[WHITE] = pieceToRemoveCount[BLACK] = 1;
            // Pursue performance at the expense of maintainability
            change_side_to_move();
            return false;
        case BoardFullAction::secondAndFirstPlayerRemovePiece:
            pieceToRemoveCount[WHITE] = pieceToRemoveCount[BLACK] = 1;
            return false;
        case BoardFullAction::sideToMoveRemovePiece:
            if (rule.isDefenderMoveFirst) {
                set_side_to_move(BLACK);
            } else {
                set_side_to_move(WHITE);
            }
            pieceToRemoveCount[sideToMove] = 1;
            return false;
        case BoardFullAction::agreeToDraw:
            set_gameover(DRAW, GameOverReason::drawBoardIsFull);
            return true;
        };
    }

    if (phase == Phase::moving && action == Action::select &&
        is_all_surrounded(sideToMove)) {
        switch (rule.stalemateAction) {
        case StalemateAction::endWithStalemateLoss:
            set_gameover(~sideToMove, GameOverReason::loseNoWay);
            return true;
        case StalemateAction::changeSideToMove:
            change_side_to_move(); // TODO(calcitem): Need?
            return false;
        case StalemateAction::removeOpponentsPieceAndMakeNextMove:
            pieceToRemoveCount[sideToMove] = 1;
            isStalemateRemoving = true;
            action = Action::remove;
            return false;
        case StalemateAction::removeOpponentsPieceAndChangeSideToMove:
            pieceToRemoveCount[sideToMove] = 1;
            action = Action::remove;
            return false;
        case StalemateAction::endWithStalemateDraw:
            set_gameover(DRAW, GameOverReason::drawNoWay);
            return true;
        }
    }

    return false;
}

int Position::calculate_mobility_diff()
{
    // TODO(calcitem): Deal with rule is no ban location
    int mobilityWhite = 0;
    int mobilityBlack = 0;

    for (Square s = SQ_BEGIN; s < SQ_END; ++s) {
        if (board[s] == NO_PIECE || board[s] == BAN_PIECE) {
            for (MoveDirection d = MD_BEGIN; d < MD_NB; ++d) {
                const Square moveSquare = MoveList<LEGAL>::adjacentSquares[s][d];
                if (moveSquare) {
                    if (board[moveSquare] & W_PIECE) {
                        mobilityWhite++;
                    }
                    if (board[moveSquare] & B_PIECE) {
                        mobilityBlack++;
                    }
                }
            }
        }
    }

    return mobilityWhite - mobilityBlack;
}

void Position::remove_ban_pieces()
{
    assert(rule.hasBannedLocations);

    for (int f = 1; f <= FILE_NB; f++) {
        for (int r = 0; r < RANK_NB; r++) {
            const auto s = static_cast<Square>(f * RANK_NB + r);

            if (board[s] == BAN_PIECE) {
                const Piece pc = board[s];
                byTypeBB[ALL_PIECES] ^= s;
                byTypeBB[type_of(pc)] ^= s;
                board[s] = NO_PIECE;
                revert_key(s);
            }
        }
    }
}

inline void Position::set_side_to_move(Color c)
{
    sideToMove = c;
    // us = c;
    them = ~sideToMove;
}

inline void Position::change_side_to_move()
{
    set_side_to_move(~sideToMove);
    st->key ^= Zobrist::side;
}

inline Key Position::update_key(Square s)
{
    const int pieceType = color_on(s);

    st->key ^= Zobrist::psq[pieceType][s];

    return st->key;
}

inline Key Position::revert_key(Square s)
{
    return update_key(s);
}

Key Position::update_key_misc()
{
    st->key = st->key << Zobrist::KEY_MISC_BIT >> Zobrist::KEY_MISC_BIT;

    // TODO: pieceToRemoveCount[sideToMove] or
    // abs(pieceToRemoveCount[sideToMove] - pieceToRemoveCount[~sideToMove])?
    st->key |= static_cast<Key>(pieceToRemoveCount[sideToMove])
              << (CHAR_BIT * sizeof(Key) - Zobrist::KEY_MISC_BIT);

    return st->key;
}

///////////////////////////////////////////////////////////////////////////////

#include "misc.h"
#include "movegen.h"

Bitboard Position::millTableBB[SQUARE_EXT_NB][LD_NB] = {{0}};

void Position::create_mill_table()
{
    Mills::mill_table_init();
}

Color Position::color_on(Square s) const
{
    return color_of(board[s]);
}

bool Position::bitboard_is_ok()
{
#ifdef BITBOARD_DEBUG
    Bitboard whiteBB = byColorBB[WHITE];
    Bitboard blackBB = byColorBB[BLACK];

    for (Square s = SQ_BEGIN; s < SQ_END; ++s) {
        if (empty(s)) {
            if (whiteBB & (1 << s)) {
                return false;
            }

            if (blackBB & (1 << s)) {
                return false;
            }
        }

        if (color_of(board[s]) == WHITE) {
            if ((whiteBB & (1 << s)) == 0) {
                return false;
            }

            if (blackBB & (1 << s)) {
                return false;
            }
        }

        if (color_of(board[s]) == BLACK) {
            if ((blackBB & (1 << s)) == 0) {
                return false;
            }

            if (whiteBB & (1 << s)) {
                return false;
            }
        }
    }
#endif

    return true;
}

int Position::potential_mills_count(Square to, Color c, Square from)
{
    int n = 0;
    Piece locbak = NO_PIECE;

    assert(SQ_0 <= from && from < SQUARE_EXT_NB);

    if (c == NOBODY) {
        c = color_on(to);
    }

    if (from >= SQ_BEGIN && from < SQ_END) {
        locbak = board[from];
        board[from] = NO_PIECE;

        CLEAR_BIT(byTypeBB[ALL_PIECES], from);
        CLEAR_BIT(byTypeBB[type_of(locbak)], from);
        CLEAR_BIT(byColorBB[color_of(locbak)], from);
    }

    const Bitboard bc = byColorBB[c];
    const Bitboard *mt = millTableBB[to];

    if ((bc & mt[LD_HORIZONTAL]) == mt[LD_HORIZONTAL]) {
        n++;
    }

    if ((bc & mt[LD_VERTICAL]) == mt[LD_VERTICAL]) {
        n++;
    }

    if ((bc & mt[LD_SLASH]) == mt[LD_SLASH]) {
        n++;
    }

    if (from >= SQ_BEGIN && from < SQ_END) {
        board[from] = locbak;

        SET_BIT(byTypeBB[ALL_PIECES], from);
        SET_BIT(byTypeBB[type_of(locbak)], from);
        SET_BIT(byColorBB[color_of(locbak)], from);
    }

    return n;
}

int Position::mills_count(Square s) const
{
    int n = 0;

    const Bitboard bc = byColorBB[color_on(s)];
    const Bitboard *mt = millTableBB[s];

    for (auto i = 0; i < LD_NB; ++i) {
        if ((bc & mt[i]) == mt[i]) {
            n++;
        }
    }

    return n;
}

bool Position::is_all_in_mills(Color c)
{
    for (Square i = SQ_BEGIN; i < SQ_END; ++i) {
        if (board[i] & static_cast<uint8_t>(make_piece(c))) {
            if (!potential_mills_count(i, NOBODY)) {
                return false;
            }
        }
    }

    return true;
}

void Position::surrounded_pieces_count(Square s, int &ourPieceCount,
                                       int &theirPieceCount, int &bannedCount,
                                       int &emptyCount) const
{
    for (MoveDirection d = MD_BEGIN; d < MD_NB; ++d) {
        const Square moveSquare = MoveList<LEGAL>::adjacentSquares[s][d];

        if (!moveSquare) {
            continue;
        }

        switch (const auto pieceType = board[moveSquare]) {
        case NO_PIECE:
            emptyCount++;
            break;
        case BAN_PIECE:
            bannedCount++;
            break;
        default:
            if (color_of(pieceType) == sideToMove) {
                ourPieceCount++;
            } else {
                theirPieceCount++;
            }
            break;
        }
    }
}

bool Position::is_all_surrounded(Color c
#ifdef MADWEASEL_MUEHLE_RULE
                                 ,
                                 Square from, Square to
#endif // MADWEASEL_MUEHLE_RULE
) const
{
    // Full
    if (pieceOnBoardCount[WHITE] + pieceOnBoardCount[BLACK] >= SQUARE_NB)
        return true;

    // Can fly
    if (pieceOnBoardCount[c] <= rule.flyPieceCount && rule.mayFly) {
        return false;
    }

    Bitboard bb = byTypeBB[ALL_PIECES];

#ifdef MADWEASEL_MUEHLE_RULE
    CLEAR_BIT(bb, from);
    SET_BIT(bb, to);
#endif // MADWEASEL_MUEHLE_RULE

    for (Square s = SQ_BEGIN; s < SQ_END; ++s) {
        if ((c & color_on(s)) && (bb & MoveList<LEGAL>::adjacentSquaresBB[s]) !=
                                     MoveList<LEGAL>::adjacentSquaresBB[s]) {
            return false;
        }
    }

    return true;
}

bool Position::is_star_square(Square s)
{
    if (rule.hasDiagonalLines == true) {
        return s == 17 || s == 19 || s == 21 || s == 23;
    }

    return s == 16 || s == 18 || s == 20 || s == 22;
}

void Position::print_board()
{
    if (rule.hasDiagonalLines) {
        printf("\n"
               "31 ----- 24 ----- 25\n"
               "| \\       |      / |\n"
               "|  23 -- 16 -- 17  |\n"
               "|  | \\    |   / |  |\n"
               "|  |  15-08-09  |  |\n"
               "30-22-14    10-18-26\n"
               "|  |  13-12-11  |  |\n"
               "|  | /    |   \\ |  |\n"
               "|  21 -- 20 -- 19  |\n"
               "| /       |      \\ |\n"
               "29 ----- 28 ----- 27\n"
               "\n");
    } else {
        printf("\n"
               "31 ----- 24 ----- 25\n"
               "|         |        |\n"
               "|  23 -- 16 -- 17  |\n"
               "|  |      |     |  |\n"
               "|  |  15-08-09  |  |\n"
               "30-22-14    10-18-26\n"
               "|  |  13-12-11  |  |\n"
               "|  |      |     |  |\n"
               "|  21 -- 20 -- 19  |\n"
               "|         |        |\n"
               "29 ----- 28 ----- 27\n"
               "\n");
    }
}

void Position::reset_bb()
{
    memset(byTypeBB, 0, sizeof(byTypeBB));
    memset(byColorBB, 0, sizeof(byColorBB));

    for (Square s = SQ_BEGIN; s < SQ_END; ++s) {
        const Piece pc = board[s];
        byTypeBB[ALL_PIECES] |= byTypeBB[type_of(pc)] |= s;
        byColorBB[color_of(pc)] |= s;
    }
}

void Position::updateMobility(MoveType mt, Square s)
{
    if (!gameOptions.getConsiderMobility()) {
        return;
    }

    const Bitboard adjacentWhiteBB = byColorBB[WHITE] &
                                     MoveList<LEGAL>::adjacentSquaresBB[s];
    const Bitboard adjacentBlackBB = byColorBB[BLACK] &
                                     MoveList<LEGAL>::adjacentSquaresBB[s];
    const Bitboard adjacentNoColorBB = (~(byColorBB[BLACK] |
                                          byColorBB[WHITE])) &
                                       MoveList<LEGAL>::adjacentSquaresBB[s];
    const int adjacentWhiteBBCount = popcount(adjacentWhiteBB);
    const int adjacentBlackBBCount = popcount(adjacentBlackBB);
    const int adjacentNoColorBBCount = popcount(adjacentNoColorBB);

    if (mt == MOVETYPE_PLACE) {
        mobilityDiff -= adjacentWhiteBBCount;
        mobilityDiff += adjacentBlackBBCount;

        if (side_to_move() == WHITE) {
            mobilityDiff += adjacentNoColorBBCount;
        } else {
            mobilityDiff -= adjacentNoColorBBCount;
        }
    } else if (mt == MOVETYPE_REMOVE) {
        mobilityDiff += adjacentWhiteBBCount;
        mobilityDiff -= adjacentBlackBBCount;

        if (color_of(board[s]) == WHITE) {
            mobilityDiff -= adjacentNoColorBBCount;
        } else {
            mobilityDiff += adjacentNoColorBBCount;
        }
    } else {
        assert(0);
    }
}

int Position::total_mills_count(Color c)
{
    assert(c == WHITE || c == BLACK);

    // TODO: Move to mills.cpp
    static const int horizontalAndVerticalLines[16][3] = {
        // Horizontal lines
        {31, 24, 25},
        {23, 16, 17},
        {15, 8, 9},
        {30, 22, 14},
        {10, 18, 26},
        {13, 12, 11},
        {21, 20, 19},
        {29, 28, 27},
        // Vertical lines
        {31, 30, 29},
        {23, 22, 21},
        {15, 14, 13},
        {24, 16, 8},
        {12, 20, 28},
        {9, 10, 11},
        {17, 18, 19},
        {25, 26, 27},
    };

    static const int diagonalLines[4][3] = {
        {31, 23, 15},
        {9, 17, 25},
        {29, 21, 13},
        {11, 19, 27},
    };

    int n = 0;

        for (int i  = 0; i < 16; i++) {
        if (color_on(static_cast<Square>(horizontalAndVerticalLines[i][0])) == c &&
            color_on(static_cast<Square>(horizontalAndVerticalLines[i][1])) ==
                c &&
            color_on(static_cast<Square>(horizontalAndVerticalLines[i][2])) ==
                c) {
            n++;
        }
    }

    if (rule.hasDiagonalLines == true) {
        for (int i = 0; i < 4; i++) {
            if (color_on(static_cast<Square>(diagonalLines[i][0])) == c &&
                color_on(static_cast<Square>(diagonalLines[i][1])) == c &&
                color_on(static_cast<Square>(diagonalLines[i][2])) == c) {
                n++;
            }
        }
    }

    return n;
}

bool Position::is_board_full_removal_at_placing_phase_end()
{
    if (rule.pieceCount == 12 &&
        rule.boardFullAction != BoardFullAction::firstPlayerLose &&
        rule.boardFullAction != BoardFullAction::agreeToDraw &&
        phase == Phase::placing && pieceInHandCount[WHITE] == 0 &&
        pieceInHandCount[BLACK] == 0 &&
        // TODO: Performance
        total_mills_count(BLACK) == 0) {
        return true;
    }

    return false;
}

bool Position::is_adjacent_to(Square s, Color c)
{
    for (int d = MD_BEGIN; d < MD_NB; d++) {
        const Square moveSquare = MoveList<LEGAL>::adjacentSquares[s][d];
        if (moveSquare != SQ_0 && color_on(moveSquare) == c) {
            return true;
        }
    }
    return false;
}

bool Position::is_stalemate_removal()
{
    if (is_board_full_removal_at_placing_phase_end()) {
        return true;
    }

    if (!(rule.stalemateAction ==
              StalemateAction::removeOpponentsPieceAndChangeSideToMove ||
          rule.stalemateAction ==
              StalemateAction::removeOpponentsPieceAndMakeNextMove)) {
        return false;
    }

    if (isStalemateRemoving == true) {
        return true;
    }

    // TODO: StalemateAction: It is best to inform the engine of this state by
    // the front end to improve performance.
    if (is_all_surrounded(sideToMove)) {
        return true;
    }

    return false;
}

} // namespace Stockfish
