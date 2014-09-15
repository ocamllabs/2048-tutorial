(*
 * Copyright (c) 2014 Daniel C. Bünzli.
 *
 * This file is distributed under the terms of the BSD3 License.
 * See the file COPYING for details.
 *)

open Gg

(* Ease in out, adapted from http://www.flong.com/texts/code/shapers_exp/ *)

let d_eps = 0.00001
let d_min_a = 0.0 +. d_eps
let d_max_a = 1.0 -. d_eps

let double_exp_sigmoid a t =
  let a = Float.clamp ~min:d_min_a ~max:d_max_a a in
  let exp = 1.0 /. (1.0 -. a) in
  if t <= 0. then 0. else
  if t >= 1. then 1. else
  if t <= 0.5 then 0.5 *. ((2.0 *. t) ** exp) else
  1.0 -. 0.5 *. ((2.0 *. (1.0 -. t)) ** exp)

let ease_in_out ?(a = 0.426) t = double_exp_sigmoid a t

(* Triangle around 0.5 *)

let tri t =
  if t <= 0. then 0. else
  if t >= 1. then 0. else
  1. -. abs_float (2. *. t -. 1.)
