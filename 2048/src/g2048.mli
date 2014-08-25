type tile

val string_of_tile : tile -> string

val empty : tile
val t2 : tile
val t4 : tile
val t8 : tile
val t16 : tile
val t32 : tile
val t64 : tile
val t128 : tile
val t256 : tile
val t512 : tile
val t1024 : tile
val t2048 : tile

type board = tile list list
type move = L | R | U | D

val shift : move -> board -> board

val is_full_board : board -> bool

val insert_into_board : tile -> board -> board option
