(** 2048 game logic. *)

(** {1 Tiles} *)

type tile
(** The type for tiles. *)

val empty : tile
(** [empty] is an empty tile. *)

val t2 : tile
(** [t2] is a tile with value 2. *)

val t4 : tile
(** [t4] is a tile with value 4. *)

val t8 : tile
(** [t8] is a tile with value 8. *)

val t16 : tile
(** [t16] is a tile with value 16. *)

val t32 : tile
(** [t32] is a tile with value 32. *)

val t64 : tile
(** [t64] is a tile with value 64. *)

val t128 : tile
(** [t128] is a tile with value 128. *)

val t256 : tile
(** [t256] is a tile with value 256. *)

val t512 : tile
(** [t512] is a tile with value 512. *)

val t1024 : tile
(** [t1024] is a tile with value 1024. *)

val t2048 : tile
(** [t2048] is a tile with value 2048. *)

val string_of_tile : tile -> string
(** [string_of_tile t] is [t] as a string. *)

(** {1 Boards} *)

type row = tile list
(** The type for board rows. *)

type board = row list
(** The type boards. *)

val insert_tile : tile -> board -> board option
(** [insert_tile tile board] is [board] with [tile] inserted in the
    first empty spot found in [board] or [None] if there was no such spot. *)

val is_board_full : board -> bool
(** [is_board_full board] is [true] iff the [board] is full. *)

(** {1 Moves} *)

(** The type for game moves. *)
type move = L | R | U | D

val shift_board : move -> board -> board
(** [shift_board move board] is the board resulting from shifting [board]
    in direction [move]. *)
