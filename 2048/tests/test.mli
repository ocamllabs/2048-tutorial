(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the BSD2 License.
 * See the file COPYING for details.
 *)

(** Tests *)

(** {1 Types} *)

type is_board_winning = G2048.board -> bool
type shift_board = G2048.move -> G2048.board -> G2048.board
type insert_square = G2048.square -> G2048.board -> G2048.board option
type is_game_over = G2048.board -> bool

(** {1 Winning boards} *)
val test_is_board_winning: is_board_winning -> unit

(** {2 Shift boards} *)

val test_is_board_winning_more: shift_board -> is_board_winning -> unit
val test_shift_empty: shift_board -> unit
val test_shift_empty_squares: shift_board -> unit
val test_shift_coalesce: shift_board -> unit
val test_shifts: shift_board -> unit
val test_shift_board_fixpoint: shift_board -> unit

(** {1 Square inserts} *)

val test_insert_row_completely_empty: insert_square -> unit
val test_insert_row_partially_empty: insert_square -> unit
val test_insert_row_full: insert_square -> unit
val test_insert_last_square: insert_square -> unit
val test_add: insert_square -> unit
val test_add_to_full: insert_square -> unit
val test_insert: insert_square -> unit

(** {1 Game over} *)

val test_game_over: is_game_over -> unit

module Make (S: G2048.Solution): sig
  val run: unit -> OUnit.test_results
end
