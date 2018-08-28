module type TicTacToe = sig

  (* [x_or_o] describes the state of a square in the 3x3 tic tac toe board
   *)
  type x_or_o

  (* [move] describes the move details for either player 1 or player 2
   *)
  type move

  (* [game_state] contains all necessary information for the moves and
   * current state of two players
   *)
  type game_state

  (* [new_state] is a completely blank game of tic tac toe
   *)
  val new_state: game_state

  (* [is_legal m] is true if the move of a chess piece is legal, false otherwise
   *)
  val is_legal: move -> bool

  (* [has_won gs] is (n,true) if player n has won the game, false otherwise
   *)
  val has_won: game_state -> int * bool

  (* [player_1_move m gs] is [gs] except that player one has committed [m],
   * altering the current state of the game
   *)
  val player_1_move: move -> game_state -> game_state

  (* [player_2_move m gs] is [gs] except that player two has committed [m],
   * altering the current state of the game
   *)
  val player_2_move: move -> game_state -> game_state

  (* [reveal_state] is a list of x_or_o list that details what is occuring
   * at each square in the board
   *)
  val reveal_state: game_state -> x_or_o list list

end

module type ConnectFour = sig

  (* [red_or_yellow] describes the state of an open area in the connect 4 grid
   *)
  type red_or_yellow

  (* [drop] describes the move details for either player 1 or player 2
   *)
  type drop

  (* [game_state] contains all necessary information for the moves and
   * current state of two players
   *)
  type game_state

  (* [new_state] is a completely blank game of connect 4; the actual grid
   * should be a 6 x 7 matrix
   *)
  val new_state: game_state

  (* [is_legal d] is true if the move of a chess piece is legal, false otherwise
   *)
  val is_legal: drop -> bool

  (* [has_won gs] is (n,true) if player n has won the game, false otherwise
   *)
  val has_won: game_state -> int * bool

  (* [player_1_drop d gs] is [gs] except that player one has committed [m],
   * altering the current state of the game
   *)
  val player_1_drop: drop -> game_state -> game_state

  (* [player_2_drop d gs] is [gs] except that player two has committed [m],
   * altering the current state of the game
   *)
  val player_2_drop: drop -> game_state -> game_state

  (* [reveal_state] is a list of red_or_yellow list that details what
   *is occuring at each square in the board
   *)
  val reveal_state: game_state -> red_or_yellow list list

end

module type Chess = sig

  (* [piece] describes any chess piece, pawn, queen, etc.
   *)
  type piece

  (* [move] describes the a move detail for a certain piece of player 1 or
   * player 2
   *)
  type move

  (* [game_state] contains all necessary information for the moves and
   * current state of two players
   *)
  type game_state

  (* [new_state] is a completely blank game of chess; pieces are all set
   * in the starting position on an 8 x 8 matrix
   *)
  val new_state: game_state

  (* [has_won gs] is (n,true) if player n has won the game, false otherwise
   *)
  val has_won: game_state -> int * bool

  (* [is_legal m] is true if the move of a chess piece is legal, false otherwise
   *)
  val is_legal: move -> bool

  (* [player_1_move m gs] is [gs] except that player one has committed [m],
   * altering the current state of the game
   *)
  val player_1_move: move -> game_state -> game_state

  (* [player_2_move m gs] is [gs] except that player two has committed [m],
   * altering the current state of the game
   *)
  val player_2_drop: move -> game_state -> game_state

  (* [reveal_state] is a list of red_or_yellow list that details what
   *is occuring at each square in the board
   *)
  val reveal_state: game_state -> piece list list

end

