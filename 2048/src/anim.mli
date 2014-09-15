(*
 * Copyright (c) 2014 Daniel C. Bünzli.
 *
 * This file is distributed under the terms of the BSD3 License.
 * See the file COPYING for details.
 *)

(** Animation curves. *)

val ease_in_out : ?a:float -> float -> float
(** [ease_in_out ~a t] is the value of an ease in and out curve at
    time [t]. Maps unit intervals to unit intervals. [a] can be used
    to control the stiffness of the curve. *)

val tri : float -> float
(** [tri t] is a triangle (tent) curve at time [t]. Maps the unit interval
    to a triangle whose maximum is at [0.5]. The height of the triangle
    is [1.], the base at [0.]. *)
