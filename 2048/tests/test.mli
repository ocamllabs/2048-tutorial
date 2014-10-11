(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the BSD2 License.
 * See the file COPYING for details.
 *)

open G2048

val test_is_board_winning: (board -> bool) -> unit

module Make (S: Solution): sig
  val run: unit -> OUnit.test_results
end
