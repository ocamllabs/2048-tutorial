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

let empty = None
let t2 = Some (2, [])
and t4 = Some (4, [])
and t8 = Some (8, [])
and t16 = Some (16, [])
and t32 = Some (32, [])
and t64 = Some (64, [])
and t128 = Some (128, [])
and t256 = Some (256, [])
and t512 = Some (512, [])
and t1024 = Some (1024, [])
and t2048 = Some (2048, [])

(* The value of the occupant of a square, if the square is occupied. *)
let square_value (sq : square) = 
  match sq with
  | None -> None
  | Some (v, _) -> Some v

let square_provenances (sq : square) =
  match sq with
  | None -> []
  | Some (_, p) -> p

let string_of_square (sq : square) =
  match sq with
  | Some (s, _) -> string_of_int s
  | None -> " "

(* Replace the provenance of a square with the no-move provenance. *)
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

(* Whether a square is occupied by a tile with the value 2048. *)
let is_square_2048 (sq : square) =
  match sq with
  | Some (2048, _) -> true
  | _ -> false

(* Select a tile to insert.  Returns t4 10% of the time and t2 90% of the time. *)
let new_square () : square =
  match Random.int 10 with
  | 0 -> t4
  | _ -> t2

(** Boards *)

(* A board is a list of lists of squares *)
type row = square list
type board = row list

let is_none s =
  match s with
  | Some _ -> false
  | None -> true

let create_board () =
  [ [empty; t2   ; empty; empty];
    [empty; empty; empty; empty];
    [empty; empty; empty; empty];
    [empty; empty; empty; t2   ]; ]

(* A board is a winning board if it contains the tile 2048. *)
let is_board_winning (b : board) =
  List.exists (List.exists is_square_2048) b

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

let find_empties = find_coordinates is_none

let rec replace_at n f l =
  match n, l with
  | _, [] -> []
  | 0, x :: xs -> f x :: xs
  | n, x :: xs -> x :: replace_at (n - 1) f xs

let replace_board_position (row, col) board square =
  let const_square _ = square in
  replace_at row (replace_at col const_square) board

(* Insert a square into an unoccupied spot on a board. *)
let insert_square (sq : square) (b : board) : board option =
  match find_empties b with
  | [] ->
     None
  | empties ->
     Some (replace_board_position
             (List.nth empties (Random.int (List.length empties))) b sq)

let board_size = Utils.listlist_dims
let fold_board = Utils.fold_listlisti

(** Moves *)

type move = L | R | U | D

(* Shift a row to the left according to the rules of the game, and
   append `empties`.  Tiles slide over empty squares, and adjacent
   tiles are coalesced if they have the same value. *)
let rec shift_left_helper (r : row) (empties : row) : row =
  match r with
  | [] ->
     empties
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

(* Shift a row in the specified direction according to the rules of the game. *)
let rec shift_board (mv : move) (b : board) : board =
  match mv with
  | L -> List.map shift_left b
  | R -> List.map shift_right b
  | U -> Utils.transpose (shift_board L (Utils.transpose b))
  | D -> Utils.transpose (shift_board R (Utils.transpose b))

(** High-level interface. *)
let game_move (mv : move) (b : board) : board =
  let b' = shift_board mv b in
  match insert_square (new_square ()) b' with
  | None -> b'
  | Some b'' -> b''

(* A row is complete if 
   * it contains no empty squares and 
   * a shift leaves it unchanged *)
let rec is_complete_row (r : row) : bool =
  let row_value r = List.map square_value r in
    not (List.exists is_none r)
 && row_value (shift_left r) = row_value r

let is_game_over (b : board) =
    List.for_all is_complete_row b
 && List.for_all is_complete_row (Utils.transpose b)
