(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the BSD2 License.
 * See the file COPYING for details.
 *)

let () = Random.self_init () (* get a seed for random numbers *)

(** Squares and tiles *)

(* The provenance of a square is a list of the previous positions and
   values of the current occupants.

   A freshly-populated square has provenance
     [].

   A square unchanged by the last move has provenance
     [{shift = 0; value = v}].

   A square occupied by a shifted tile has provenance
     [{shift = s; value = v}].

   A square occupied by combining two tiles has provenance
     [{shift = s1; value = v}; {shift = s2; value = v}].
*)
type provenance = { shift : int; value : int }


(* A tile is represented as its value. *)
type tile = int * provenance list

(* An unoccupied square is represented as None.
   A square occupied by a tile t is represented as Some t. *)
type square = tile option

(* A board is a list of lists of squares *)
type row = square list
type board = row list

type move = L | R | U | D

module type Solution = sig
  val is_square_2048: square -> bool
  val is_complete_row: row -> bool
  val is_board_winning : board -> bool
  val insert_square : square -> board -> board option
  val shift_left_helper: row -> row -> row
  val shift_board : move -> board -> board
  val is_game_over : board -> bool
  val square_provenances: square -> provenance list
end

let empty = None
let t2 = Some (2, [])
let t4 = Some (4, [])
let t8 = Some (8, [])
let t16 = Some (16, [])
let t32 = Some (32, [])
let t64 = Some (64, [])
let t128 = Some (128, [])
let t256 = Some (256, [])
let t512 = Some (512, [])
let t1024 = Some (1024, [])
let t2048 = Some (2048, [])

let square_value (sq : square) =
  match sq with
  | None -> None
  | Some (v, _) -> Some v

let string_of_square x = match square_value x with
| Some s -> string_of_int s
| None -> " "

(* Select a tile to insert.  Returns t4 10% of the time and t2 90% of the time. *)
let new_square () : square =
  match Random.int 10 with
  | 0 -> t4
  | _ -> t2

(** Boards *)

let create_board () =
  [ [empty; t2   ; empty; empty];
    [empty; empty; empty; empty];
    [empty; empty; empty; empty];
    [empty; empty; empty; t2   ]; ]

let board_size = Utils.listlist_dims
let fold_board = Utils.fold_listlisti

let clear_provenance (sq : square) =
  match sq with
  | None -> None
  | Some (t, _) -> Some (t, [{ shift = 0; value = t}] )

(* Update the provenance of a square after a shift. *)
let shift_square t =
  match t with
  | None -> None
  | Some (t, []) -> Some(t, [{ shift = 1; value = t }])
  | Some (t, m :: _) -> Some (t, [{ m with shift = m.shift + 1 }])

let find_positions p l =
  let f (positions, i) x =
    let positions' =
      match p x with
      | true -> i :: positions
      | false -> positions
    in
    (positions', succ i)
  in
  let positions, _ = List.fold_left f ([], 0) l in
  List.rev positions

let find_coordinates p b =
  let f row r =
    let g col = (row, col) in
    List.map g (find_positions p r)
  in
  List.concat (List.mapi f b)

let is_none = function None -> true | _ -> false

let find_empties = find_coordinates is_none

let rec replace_at n f l =
  match n, l with
  | _, [] -> []
  | 0, x :: xs -> f x :: xs
  | n, x :: xs -> x :: replace_at (n - 1) f xs

let replace_board_position (row, col) board square =
  let const_square _ = square in
  replace_at row (replace_at col const_square) board

(** Moves *)

module Make (S: Solution) = struct

  include S

  (** High-level interface. *)
  let game_move (mv : move) (b : board) : board =
    let b' = S.shift_board mv b in
    match S.insert_square (new_square ()) b' with
    | None -> b'
    | Some b'' -> b''

end

module Default = struct

  type tile = int

  (********* Step 1 *********)

  (* Your first task is to fix the code so that the tests pass. *)

  (* TODO: Complete the function `is_square_2048`. The function should
     return `true` if a square has the value `2048` and `false`
     otherwise. *)
  let is_square_2048 (sq : square) = match sq with Some (2048, _) -> true | _ -> false

  (* TODO: * Write the `is_board_winning. The `List.exists` function
     (which you can try out in an IOCaml notebook) may prove
     useful. *)
  let is_board_winning (b : board) =  List.exists (List.exists is_square_2048) b

  (* At this point you should be able to run the tests again to check
     that your implementation is correct. *)

  (********* Step 2 *********)

  (* The next step is to implement the logic for sliding boards up,
     down, left and right. *)

  (* TODO: Implement the `shift_left_helper` to support the left shift
     action.  You'll need to consider the following cases:

     - The row is empty.  There's nothing to do except return the
     accumulated `empties` list.

     - The first square is unoccupied (`None`).  Add it to `empties`
       and process the rest of the row.

     - The first two squares are occupied by equal tiles.  Merge them
       together, add an entry to the `empties` list, and process the
       rest of the row.

     - The first square is occupied, but the second square is
       unoccupied.  Move the unoccupied square to the `empties` list
       and reprocess the row.

     - The first square is occupied and not covered by the cases
       above.  Move on to processing the rest of the list.

     Hint: use pattern matching on r and recursion. *)
  let rec shift_left_helper (r : row) (empties : row) : row =
    match r with
    | [] -> empties
    | None :: rest ->
        shift_left_helper (List.map shift_square rest) (None :: empties)
    | Some (x, xprov) :: Some (y, {shift; value} :: _) :: rest when x = y ->
        Some (x + y, xprov @ [{shift = shift + 1; value}])
        :: shift_left_helper (List.map shift_square rest) (None :: empties)
    | Some x :: None :: rest ->
        shift_left_helper (Some x :: List.map shift_square rest) (None :: empties)
    | Some (x, prov) :: r ->
        Some (x, prov) :: shift_left_helper r empties

  let shift_left (r : row) = shift_left_helper (List.map clear_provenance r) []
  let shift_right l = List.rev (shift_left (List.rev l))

  (* TODO: Implement the `shift_board` function using
     `shift_left_helper`.  Hint: how can you implement a right shift
     in terms of a left shift?  How can you implement an up shift in
     terms of a left shift? *)
  (* Shift a row in the specified direction according to the rules of the game. *)
  let rec shift_board (mv : move) (b : board) : board =
    match mv with
    | L -> List.map shift_left b
    | R -> List.map shift_right b
    | U -> Utils.transpose (shift_board L (Utils.transpose b))
    | D -> Utils.transpose (shift_board R (Utils.transpose b))

  (* TODO.  Hint: use pattern matching on mv and shift_left, List.rev
     and Utils.transpose. *)

  (********* Step 3 *********)

  (* The next step is to implement a function for adding new tiles to
     the board after a move. *)

  (* TODO: Implement the `insert_square` function.  You may like to
     start by implementing a function `insert_into_row`, perhaps using
     `Utils.replace_one`.  You may find it simplest to simply insert
     the tile in the first empty space.  There'll be an opportunity
     for a more realistic implementation in step 6. *)
  let insert_square (sq : square) (b : board) : board option =
    match find_empties b with
    | [] -> None
    | empties ->
        Some (replace_board_position
                (List.nth empties (Random.int (List.length empties))) b sq)

  (* There's a minor milestone at this point: if the tests pass then
     the game should be somewhat playable.  (The sliding animations
     won't appear until you've completed step 5.)  You can try out the
     game by loading `2048/_build/src/2048.html` in a browser. *)

  (********* Step 4 *********)

  (* You've written have a check for a winning board, but we don't yet
     have a way to check whether the game has been lost.  The game is
     lost when it's no longer possible to make a move. *)

  (* TODO: Write a function `is_complete_row`.  A row is considered
     complete if there are no empty squares and if a shift leaves it
     unchanged. *)
  let rec is_complete_row (r : row) : bool =
    let row_value r = List.map square_value r in
    not (List.exists is_none r)
    && row_value (shift_left r) = row_value r

  (* TODO: Using `is_complete_row`, write a function `is_game_over`.
     Don't forget to run the tests! *)
  let is_game_over (b : board) =
    List.for_all is_complete_row b
    && List.for_all is_complete_row (Utils.transpose b)

  (********* Step 5 *********)

  (* At this point it's possible to play the game, but the tiles leap
     disconcertingly around the board rather than sliding smoothly.
     Sliding animations require keeping track of where tiles came
     from: their *provenance*. *)

  (* TODO: Change the definition of the `tile` type in `g2048.ml` to
     include provenance: {| type tile = int * provenance list |}

     You'll need to reorder the type definitions so that `provenance`
     is defined before `tile`.  *)
  let square_provenances (sq : square) =
    match sq with
    | None -> []
    | Some (_, p) -> p

  (* TODO: Update the function `square_provenances` to return the
     actual provenance (where available) rather than an empty list. *)

  (* TODO: Update the shift functions (`shift_left` etc.) to keep
     track of provenance. *)

  (* TODO: Update any other functions (e.g. `string_of_square`) which
     no longer compile with the new definition of `tile`. *)

  (* Once the provenance tests pass you can run the game again and see
     the sliding animations in action! *)

  (********* Step 5 *********)

  (* Always inserting squares in the first empty space makes the game
     much less challenging.  See if you can update `insert_square` to
     use a random empty position instead (perhaps using
     `Utils.replace_at`).  Don't forget to check that the tests still
     pass! *)

end
