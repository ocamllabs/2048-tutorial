(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the BSD2 License.
 * See the file COPYING for details.
 *)

module Make (S: G2048.Solution): sig
  val run: unit -> OUnit.test_results
end
