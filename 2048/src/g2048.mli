(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the BSD2 License.
 * See the file COPYING for details.
 *)

(** 2048 game logic. *)

(** {1 Types} *)

type provenance = { shift : int; value : int }
(** The provenance of a tile. {e Not used by the online tutorial.} *)

type square = (int * provenance list) option
(** The type for squares. *)

type row = square list
(** The type for board rows. *)

type board = row list
(** The type for boards. *)

(** The type for game moves.
    {ul
    {- [L] when the player press the left arrow.}
    {- [R] when the player press the up arrow.}
    {- [U] when the player press the up arrow.}
    {- [D] when the player press the down arrow.}}
*)
type move = L | R | U | D

(** {2 Squares} *)

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

(** {2 Boards} *)

val create_board : unit -> board
(** [create_board ()] is a new board. *)

val board_size : board -> int * int
(** [board_size board] is the number of columns and rows in the board. *)

val fold_board : ('a -> (int * int) -> square -> 'a) -> 'a -> board -> 'a
(** [fold_board f acc board] folds [f] over all the squares of [board] with
    the zero-based positions and starting with [acc]. The left-bottom
    corner has position [(0,0)]. *)

(** {1 Solutions} *)

module type Solution = sig

  val is_square_2048: square -> bool
  (** Whether a square is occupied by a tile with the value 2048. *)

  val is_complete_row: row -> bool
  (** A row is complete if
     * it contains no empty squares and
     * a shift leaves it unchanged *)

  val is_board_winning : board -> bool
  (** [is_board_winning board] is [true] iff the [board] is a winning board. *)

  val insert_square : square -> board -> board option
  (** [insert_square square board] is [board] with [square] inserted
      in an empty spot or [None] if there was no such spot. *)

  val shift_left_helper: row -> row -> row
  (** Shift a row to the left according to the rules of the game, and
      append `empties`.  Tiles slide over empty squares, and adjacent
      tiles are coalesced if they have the same value. *)

  val shift_board : move -> board -> board
  (** [shift_board move board] is the board resulting from shifting
      [board] in direction [move]. *)

  val is_game_over : board -> bool
  (** [is_game_over board] is [true] iff there are no valid moves. *)

  val square_provenances: square -> provenance list
  (** [square_provenances s] are the tile provenances on a square. {e Not
      used by the online tutorial.} *)

end

module Default: Solution

module Make (S: Solution): sig

  include Solution

  val game_move : move -> board -> board
  (** [game_move move board] is the board resulting from shifting [board]
      in direction [move] and inserts a new square if the board is not full. *)

end
