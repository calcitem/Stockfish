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
#include <cmath>
#include <iostream>
#include <sstream>
#include <string>

#include "benchmark.h"
#include "evaluate.h"
#include "movegen.h"
#include "position.h"
#include "search.h"
#include "thread.h"
#include "timeman.h"
#include "tt.h"
#include "uci.h"
#include "nnue/evaluate_nnue.h"

#include "config.h"

using namespace std;

namespace Stockfish {

namespace {

    // FEN string of the initial position, normal mill game
    const char* StartFEN9 = "********/********/******** w p p 0 9 0 9 0 0 1";
    const char* StartFEN10 = "********/********/******** w p p 0 10 0 10 0 0 1";
    const char* StartFEN11 = "********/********/******** w p p 0 11 0 11 0 0 1";
    const char* StartFEN12 = "********/********/******** w p p 0 12 0 12 0 0 1";

    char StartFEN[BUFSIZ];

  // position() is called when the engine receives the "position" UCI command.
  // It sets up the position that is described in the given FEN string ("fen") or
  // the initial position ("startpos") and then makes the moves given in the following
  // move list ("moves").

  void position(Position& pos, istringstream& is, StateListPtr& states) {

    Move m;
    string token, fen;

    is >> token;

    if (token == "startpos")
    {
        fen = StartFEN;
        is >> token; // Consume the "moves" token, if any
    }
    else if (token == "fen")
        while (is >> token && token != "moves")
            fen += token + " ";
    else
        return;

    states = StateListPtr(new std::deque<StateInfo>(1)); // Drop the old state and create a new one
    pos.set(fen, &states->back(), Threads.main());

    // Parse the move list, if any
    while (is >> token && (m = UCI::to_move(pos, token)) != MOVE_NONE)
    {
        states->emplace_back();
        pos.do_move(m, states->back());
    }

   // TODO: Sanmill: Stockfish does not have this
    Threads.main()->us = pos.side_to_move();
  }

  // trace_eval() prints the evaluation of the current position, consistent with
  // the UCI options set so far.

  void trace_eval(Position& pos) {

    StateListPtr states(new std::deque<StateInfo>(1));
    Position p;
    p.set(pos.fen(), &states->back(), Threads.main());

    Eval::NNUE::verify();

    sync_cout << "\n" << Eval::trace(p) << sync_endl;
  }


  // setoption() is called when the engine receives the "setoption" UCI command.
  // The function updates the UCI option ("name") to the given value ("value").

  void setoption(istringstream& is) {

    string token, name, value;

    is >> token; // Consume the "name" token

    // Read the option name (can contain spaces)
    while (is >> token && token != "value")
        name += (name.empty() ? "" : " ") + token;

    // Read the option value (can contain spaces)
    while (is >> token)
        value += (value.empty() ? "" : " ") + token;

    if (Options.count(name))
        Options[name] = value;
    else
        sync_cout << "No such option: " << name << sync_endl;
  }


  // go() is called when the engine receives the "go" UCI command. The function
  // sets the thinking time and other parameters from the input string, then starts
  // with a search.

  void go(Position& pos, istringstream& is, StateListPtr& states) {

 #ifdef UCI_AUTO_RE_GO
  begin:
#endif

    Search::LimitsType limits;
    string token;
    bool ponderMode = false;

    limits.startTime = now(); // The search starts as early as possible

    while (is >> token)
        if (token == "searchmoves") // Needs to be the last command on the line
            while (is >> token)
                limits.searchmoves.push_back(UCI::to_move(pos, token));

        else if (token == "wtime")     is >> limits.time[WHITE];
        else if (token == "btime")     is >> limits.time[BLACK];
        else if (token == "winc")      is >> limits.inc[WHITE];
        else if (token == "binc")      is >> limits.inc[BLACK];
        else if (token == "movestogo") is >> limits.movestogo;
        else if (token == "depth")     is >> limits.depth;
        else if (token == "nodes")     is >> limits.nodes;
        else if (token == "movetime")  is >> limits.movetime;
        else if (token == "mate")      is >> limits.mate;
        else if (token == "perft")     is >> limits.perft;
        else if (token == "infinite")  limits.infinite = 1;
        else if (token == "ponder")    ponderMode = true;

    Threads.start_thinking(pos, states, limits, ponderMode);

    if (pos.get_phase() == Phase::gameOver) {
#ifdef UCI_AUTO_RESTART
        // TODO(calcitem)
        while (true) {
            if (Threads.main()->searching == true) {
                continue;
            }

            pos->set(StartFEN, Threads.main());
            Threads.main()->us = WHITE; // WAR
            break;
        }
#else
        return;
#endif
    }

#ifdef UCI_AUTO_RE_GO
    goto begin;
#endif
  }


  // bench() is called when the engine receives the "bench" command.
  // Firstly, a list of UCI commands is set up according to the bench
  // parameters, then it is run one by one, printing a summary at the end.

  void bench(Position& pos, istream& args, StateListPtr& states) {

    string token;
    uint64_t num, nodes = 0, cnt = 1;

    vector<string> list = setup_bench(pos, args);
    num = count_if(list.begin(), list.end(), [](const string& s) { return s.find("go ") == 0 || s.find("eval") == 0; });

    TimePoint elapsed = now();

    for (const auto& cmd : list)
    {
        istringstream is(cmd);
        is >> skipws >> token;

        if (token == "go" || token == "eval")
        {
            cerr << "\nPosition: " << cnt++ << '/' << num << " (" << pos.fen() << ")" << endl;
            if (token == "go")
            {
               go(pos, is, states);
               Threads.main()->wait_for_search_finished();
               nodes += Threads.nodes_searched();
            }
            else
               trace_eval(pos);
        }
        else if (token == "setoption")  setoption(is);
        else if (token == "position")   position(pos, is, states);
        else if (token == "ucinewgame") { Search::clear(); elapsed = now(); } // Search::clear() may take a while
    }

    elapsed = now() - elapsed + 1; // Ensure positivity to avoid a 'divide by zero'

    dbg_print();

    cerr << "\n==========================="
         << "\nTotal time (ms) : " << elapsed
         << "\nNodes searched  : " << nodes
         << "\nNodes/second    : " << 1000 * nodes / elapsed << endl;
  }

  // The win rate model returns the probability of winning (in per mille units) given an
  // eval and a game ply. It fits the LTC fishtest statistics rather accurately.
  int win_rate_model(Value v, int ply) {

     // The model only captures up to 240 plies, so limit the input and then rescale
     double m = std::min(240, ply) / 64.0;

     // The coefficients of a third-order polynomial fit is based on the fishtest data
     // for two parameters that need to transform eval to the argument of a logistic
     // function.
     constexpr double as[] = {   0.33677609,   -4.30175627,   33.08810557,  365.60223431};    // TODO: Sanmill
     constexpr double bs[] = {  -2.50471102,   14.23235405,  -14.33066859,   71.42705250 };    // TODO: Sanmill

     // Enforce that NormalizeToPawnValue corresponds to a 50% win rate at ply 64
     static_assert(UCI::NormalizeToPawnValue == int(as[0] + as[1] + as[2] + as[3]));

     double a = (((as[0] * m + as[1]) * m + as[2]) * m) + as[3];
     double b = (((bs[0] * m + bs[1]) * m + bs[2]) * m) + bs[3];

     // Transform the eval to centipawns with limited range
     double x = std::clamp(double(v), -4000.0, 4000.0);

     // Return the win rate in per mille units rounded to the nearest value
     return int(0.5 + 1000 / (1 + std::exp((a - x) / b)));
  }

} // namespace


/// UCI::loop() waits for a command from the stdin, parses it and then calls the appropriate
/// function. It also intercepts an end-of-file (EOF) indication from the stdin to ensure a
/// graceful exit if the GUI dies unexpectedly. When called with some command-line arguments,
/// like running 'bench', the function returns immediately after the command is executed.
/// In addition to the UCI ones, some additional debug commands are also supported.

void UCI::loop(int argc, char* argv[]) {

  Position pos;
  string token, cmd;
  StateListPtr states(new std::deque<StateInfo>(1));

  #ifdef _MSC_VER
  switch (rule.pieceCount) {
  case 9:
     strncpy_s(StartFEN, BUFSIZ, StartFEN9, BUFSIZ - 1);
     break;
  case 10:
     strncpy_s(StartFEN, BUFSIZ, StartFEN10, BUFSIZ - 1);
     break;
  case 11:
     strncpy_s(StartFEN, BUFSIZ, StartFEN11, BUFSIZ - 1);
     break;
  case 12:
     strncpy_s(StartFEN, BUFSIZ, StartFEN12, BUFSIZ - 1);
     break;
  default:
     assert(0);
     break;
  }
#else
  switch (rule.pieceCount) {
  case 9:
     strncpy(StartFEN, StartFEN9, BUFSIZ - 1);
     break;
  case 10:
     strncpy(StartFEN, StartFEN10, BUFSIZ - 1);
     break;
  case 11:
     strncpy(StartFEN, StartFEN11, BUFSIZ - 1);
     break;
  case 12:
     strncpy(StartFEN, StartFEN12, BUFSIZ - 1);
     break;
  default:
     assert(0);
     break;
  }
#endif

  StartFEN[BUFSIZ - 1] = '\0';

  pos.set(StartFEN, &states->back(), Threads.main());

  for (int i = 1; i < argc; ++i)
      cmd += std::string(argv[i]) + " ";

  do {
#ifdef FLUTTER_UI
      static const int LINE_INPUT_MAX_CHAR = 4096;
      char line[LINE_INPUT_MAX_CHAR];
      CommandChannel* channel = CommandChannel::getInstance();
      while (!channel->popupCommand(line))
        Idle();
      cmd = line;
      LOGD("[uci] input: %s\n", line);
#else
      if (argc == 1 && !getline(cin, cmd)) // Wait for an input or an end-of-file (EOF) indication
          cmd = "quit";
#endif

      istringstream is(cmd);

      token.clear(); // Avoid a stale if getline() returns nothing or a blank line
      is >> skipws >> token;

      if (    token == "quit"
          ||  token == "stop")
          Threads.stop = true;

      // The GUI sends 'ponderhit' to tell that the user has played the expected move.
      // So, 'ponderhit' is sent if pondering was done on the same move that the user
      // has played. The search should continue, but should also switch from pondering
      // to the normal search.
      else if (token == "ponderhit")
          Threads.main()->ponder = false; // Switch to the normal search

      else if (token == "uci")
          sync_cout << "id name " << engine_info(true)
                    << "\n"       << Options
                    << "\nuciok"  << sync_endl;

      else if (token == "setoption")  setoption(is);
      else if (token == "go")         go(pos, is, states);
      else if (token == "position")   position(pos, is, states);
      else if (token == "ucinewgame") Search::clear();
      else if (token == "isready")    sync_cout << "readyok" << sync_endl;

      // Add custom non-UCI commands, mainly for debugging purposes.
      // These commands must not be used during a search!
      else if (token == "flip")     pos.flip();
      else if (token == "bench")    bench(pos, is, states);
      else if (token == "d")        sync_cout << pos << sync_endl;
      else if (token == "eval")     trace_eval(pos);
      else if (token == "compiler") sync_cout << compiler_info() << sync_endl;
      else if (token == "export_net")
      {
          std::optional<std::string> filename;
          std::string f;
          if (is >> skipws >> f)
              filename = f;
          Eval::NNUE::save_eval(filename);
      }
      else if (token == "--help" || token == "help" || token == "--license" || token == "license")
          sync_cout << "\nStockfish is a powerful chess engine for playing and analyzing."    // TODO: Sanmill
                       "\nIt is released as free software licensed under the GNU GPLv3 License."
                       "\nStockfish is normally used with a graphical user interface (GUI) and implements"    // TODO: Sanmill
                       "\nthe Universal Chess Interface (UCI) protocol to communicate with a GUI, an API, etc."
                       "\nFor any further information, visit https://github.com/official-stockfish/Stockfish#readme"    // TODO: Sanmill
                       "\nor read the corresponding README.md and Copying.txt files distributed along with this program.\n" << sync_endl;
      else if (!token.empty() && token[0] != '#')
          sync_cout << "Unknown command: '" << cmd << "'. Type help for more information." << sync_endl;

  } while (token != "quit" && argc == 1); // The command-line arguments are one-shot
}


/// UCI::value() converts a Value to a string by adhering to the UCI protocol specification:
///
/// cp <x>    The score from the engine's point of view in centipawns.
/// mate <y>  Mate in 'y' moves (not plies). If the engine is getting mated,
///           uses negative values for 'y'.

string UCI::value(Value v) {

  assert(-VALUE_INFINITE < v && v < VALUE_INFINITE);

  stringstream ss;

  if (abs(v) < VALUE_MATE_IN_MAX_PLY)
      ss << "cp " << v * 100 / NormalizeToPawnValue;
  else
      ss << "mate " << (v > 0 ? VALUE_MATE - v + 1 : -VALUE_MATE - v) / 2;

  return ss.str();
}


/// UCI::wdl() reports the win-draw-loss (WDL) statistics given an evaluation
/// and a game ply based on the data gathered for fishtest LTC games.

string UCI::wdl(Value v, int ply) {

  stringstream ss;

  int wdl_w = win_rate_model( v, ply);
  int wdl_l = win_rate_model(-v, ply);
  int wdl_d = 1000 - wdl_w - wdl_l;
  ss << " wdl " << wdl_w << " " << wdl_d << " " << wdl_l;

  return ss.str();
}


/// UCI::square() converts a Square to a string in algebraic notation (g1, a7, etc.)

std::string UCI::square(Square s) {
  return std::string { '(', static_cast<char>('0' + file_of(s)), ',',
      static_cast<char>('0' + rank_of(s)), ')' };
}


/// UCI::move() converts a Move to a string in coordinate notation (g1f3, a7a8q).
/// The only special case is castling where the e1g1 notation is printed in
/// standard chess mode.     // TODO: Sanmill
/// Internally, all castling moves are always encoded as 'king captures rook'.

string UCI::move(Move m) {

    string move;

  const Square to = to_sq(m);

  if (m == MOVE_NONE)
      return "(none)";

  if (m == MOVE_NULL)
      return "0000";

  if (m < 0) {
      move = "-" + square(to);
  } else if (m & 0x7f00) {
      const Square from = from_sq(m);
      move = square(from) + "->" + square(to);
  } else {
      move = square(to);
  }

  return move;
}


/// UCI::to_move() converts a string representing a move in coordinate notation
/// (g1f3, a7a8q) to the corresponding legal Move, if any.

Move UCI::to_move(Position& pos, string& str) {

  for (const auto& m : MoveList<LEGAL>(pos))
      if (str == UCI::move(m))
          return m;

  return MOVE_NONE;
}

} // namespace Stockfish
