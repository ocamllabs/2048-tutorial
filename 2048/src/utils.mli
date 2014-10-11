(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the BSD2 License.
 * See the file COPYING for details.
 *)

(** Utility functions. *)

val transpose : 'a list list -> 'a list list
(** Transpose a matrix. For instance:

  {[
      [ [ 1; 2];
        [ 3; 4]; ]
  ]}

  becomes:

  {[
      [ [ 1; 3];
        [ 2; 4]; ]
  ]}
*)

val replace_one : ('a -> 'a option) -> 'a list -> 'a list option
(** [replace_one f l] replace the first element [e] in [l] such that
    [f e] is not [None]. The new value is [y] if [f x = Some y]. *)

(** / **)

val fold_listlisti : ('a -> int * int -> 'b -> 'a) -> 'a -> 'b list list -> 'a
val listlist_dims : 'a list list -> int * int
val replace_at : int -> ('a -> 'a) -> 'a list -> 'a list
