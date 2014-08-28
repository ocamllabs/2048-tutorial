(** 2048 game logic. *)

(** {1 Square} *)

type square
(** The type for squares. *)

val empty : square
(** [empty] is an empty square. *)

val t2 : square
(** [t2] is a square with value 2. *)

val t4 : square
(** [t4] is a square with value 4. *)

val t8 : square
(** [t8] is a square with value 8. *)

val t16 : square
(** [t16] is a square with value 16. *)

val t32 : square
(** [t32] is a square with value 32. *)

val t64 : square
(** [t64] is a square with value 64. *)

val t128 : square
(** [t128] is a square with value 128. *)

val t256 : square
(** [t256] is a square with value 256. *)

val t512 : square
(** [t512] is a square with value 512. *)

val t1024 : square
(** [t1024] is a square with value 1024. *)

val t2048 : square
(** [t2048] is a square with value 2048. *)

val square_value : square -> int option
(** [square_value t] is [t]'s value (if any). *)

val string_of_square : square -> string
(** [string_of_square t] is [t] as a string. *)

val is_new_tile : square -> bool
(** [is_new_tile t] indicates whether [t] is freshly inserted. *)

(** {1 Provenance} *)
type provenance = { shift : int; value : int }
(** The provenance of a tile *)

val square_provenances : square -> provenance list
(** [square_provenances s] are the tile provenances on a square. *)

(** {1 Boards} *)

type row = square list
(** The type for board rows. *)

type board = row list
(** The type boards. *)

val create_board : unit -> board
(** [create_board ()] is a new board. *)

val insert_square : square -> board -> board option
(** [insert_square square board] is [board] with [square] inserted in the
    first empty spot found in [board] or [None] if there was no such spot. *)

val insert_random_square : square -> board -> board option
(** [insert_random_square square board] is [board] with [square] inserted
    in a randomly-chosen empty spot or [None] if there was no such spot. *)

val is_board_full : board -> bool
(** [is_board_full board] is [true] iff the [board] is full. *)

val is_game_over : board -> bool
(** [is_game_over board] is [true] iff there are no valid moves. *)

val is_board_winning : board -> bool
(** [is_board_winning board] is [true] iff the [board] is a winning board. *)

val board_size : board -> int * int
(** [board_size board] is the number of columns and rows in the board. *)

val fold_board : ('a -> (int * int) -> square -> 'a) -> 'a -> board -> 'a
(** [fold_board f acc board] folds [f] over all the squares of [board] with
    the zero-based positions and starting with [acc]. The left-bottom
    corner has position [(0,0)]. *)

(** {1 Moves} *)

(** The type for game moves. *)
type move = L | R | U | D

val shift_board : move -> board -> board
(** [shift_board move board] is the board resulting from shifting [board]
    in direction [move]. *)

val game_move : move -> board -> board
(** [game_move move board] is the board resulting from shifting [board]
    in direction [move] and inserts a new square if the board is not full. *)

val last_move_score : board -> int
(** [last_move_score board] is the score achieved by the last move. *)
