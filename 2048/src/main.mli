(*
 * Copyright (c) 2014 Daniel C. Bünzli.
 *
 * This file is distributed under the terms of the BSD2 License.
 * See the file COPYING for details.
 *)

module Make (S: G2048.Solution): sig

  val start: unit -> unit
  (** Start the game *)

end
