(* There are two operations:

   * shifting the board in some direction, which always succeeds (although
     it's sometimes the identity)

   * populating an empty square, which may fail
*)

(* Types.  We take positions rather than tiles as fundamental -- i.e. a
   position contains a tiles; a tile does not have coordinates. *)
type square = int option
type row = square list
type board = row list
type move = L | R | U | D

let empty = None

let t2 = Some 2
and t4 = Some 4
and t8 = Some 8
and t16 = Some 16
and t32 = Some 32
and t64 = Some 64
and t128 = Some 128
and t256 = Some 256
and t512 = Some 512
and t1024 = Some 1024
and t2048 = Some 2048

let string_of_square = function
  | Some s -> string_of_int s
  | None -> " "

(* Let's start with the shifts. *)

(* [shift_left] is the central function, and defines the change in a single
   row under a left shift.  The other three shifts can be defined in terms of
   [shift_left] (see [shift]); [shift_left_helper] is an auxiliary function
   that accumulates empty squares. *)
let rec shift_left_helper : row -> row -> row =
  (* invariant: length list + length accum = length rhs.
     Alternatively: everything on the left must appear on the right, including accum.
  *)
  fun list empties -> match list with
  | [] ->
    empties 
  | Some x :: [] ->
    Some x :: empties
  | None :: rest ->
    shift_left_helper rest (None :: empties)
  | Some x :: None :: rest ->
    shift_left_helper (Some x :: rest) (None :: empties)
  | Some x :: Some y :: rest when x = y ->
    Some (x + y) :: shift_left_helper rest (None :: empties)
  | Some x :: Some y :: rest ->
    Some x :: shift_left_helper (Some y :: rest) empties

(* [shift_left] starts with an empty accumulator *)
let shift_left l = shift_left_helper l []
let shift_right l = List.rev (shift_left (List.rev l))

(* This is partial.  There's also an implementation in Jane Street's Core. *)
let rec transpose = function
  | [] -> []
  | [] :: _ -> []
  | x -> List.(map hd x :: transpose (map tl x))

let rec shift : move -> board -> board = fun move board ->
  match move with
  | L -> List.map shift_left board
  | R -> List.map shift_right board
  | U -> transpose (shift L (transpose board))
  | D -> transpose (shift R (transpose board))

(* On to the insertion.  First, some functions for determining whether the
   board is full.  *)

(* (This is also in Core.) *)
let is_some = function
  | None -> false
  | Some _ -> true

let is_full_row r = List.for_all is_some r
let is_full_board b = List.for_all is_full_row b

(* optionally replace a single item in a list *)
let rec replace_one p l = match l with
  | [] -> None
  | x :: xs ->
    begin match p x with
    | Some y -> Some (y :: xs)
    | None ->
      begin match replace_one p xs with
      | None -> None
      | Some ys -> Some (x :: ys)
      end
    end

(* Populate the first empty spot in a row. *)
let insert_into_row (sq : square) (l : row) : row option = replace_one
  (function None -> Some sq | Some _ -> None) l

(* Populate the first empty spot on a board. *)
let insert_into_board sq : board -> board option
  = replace_one (insert_into_row sq)
