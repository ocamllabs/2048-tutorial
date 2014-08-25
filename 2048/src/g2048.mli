type square

val string_of_square : square -> string

val empty : square

val t2 : square
val t4 : square
val t8 : square
val t16 : square
val t32 : square
val t64 : square
val t128 : square
val t256 : square
val t512 : square
val t1024 : square
val t2048 : square

type board = square list list
type move = L | R | U | D

val shift : move -> board -> board

val is_full_board : board -> bool

val insert_into_board : square -> board -> board option
