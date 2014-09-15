(*
 * Copyright (c) 2014 Daniel C. Bünzli.
 *
 * This file is distributed under the terms of the BSD3 License.
 * See the file COPYING for details.
 *)

(** 2048 game board renderer *)

(** {1 Rendering boards} *)

val image_of_board : G2048.board -> Vg.image
(** [image_of_board board] is an image from [board]. *)

val animate_board : float -> G2048.move option -> G2048.board -> Vg.image
(** [animate_board t move board] is the image of the animation at time
    [t] of the move [move] that yielded [board]. If [t] is [0.] we
    have a still rendering of the previous board if [t] is [1.] we
    have a still rendering of the new board. Also overlays game over
    or winning conditions. *)
