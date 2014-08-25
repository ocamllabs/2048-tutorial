open G2048

(** Formatting for boards *)
let repeat_string n s =
  let len = String.length s in
  let buf = Buffer.create (n * len) in
  for i = 1 to n do Buffer.add_string buf s done;
  Buffer.contents buf

let string_of_board b =
  let pad_tile t = Printf.sprintf "%5s " (string_of_square t) in
  let format_row row =
    Printf.sprintf "|%s|" (String.concat "" (List.map pad_tile row))
  in
  let hline = repeat_string (List.length b) "------" in
  let body = String.concat "\n" (List.map format_row b) in
  hline ^ "\n" ^ body ^ "\n" ^ hline

(** Generating random boards *)
let non_empty_tiles = [t2; t4; t8; t16; t32; t64; t128; t256; t512; t1024; t2048]
let tiles = empty :: non_empty_tiles

let const x _ = x
let arbitrary_tile : square QCheck.Arbitrary.t =
  QCheck.Arbitrary.among tiles
let arbitrary_non_empty_tile : square QCheck.Arbitrary.t =
  QCheck.Arbitrary.among non_empty_tiles
let arbitrary_row ~len : square list QCheck.Arbitrary.t =
  QCheck.Arbitrary.list ~len:(const len) arbitrary_tile
let arbitrary_full_row ~len : square list QCheck.Arbitrary.t =
  QCheck.Arbitrary.list ~len:(const len) arbitrary_non_empty_tile
let arbitrary_board ?(size=4) : board QCheck.Arbitrary.t =
  QCheck.Arbitrary.(list ~len:(const size) (arbitrary_row ~len:size))
let arbitrary_full_board ?(size=4) : board QCheck.Arbitrary.t =
  QCheck.Arbitrary.(list ~len:(const size) (arbitrary_full_row ~len:size))

let rec iter n f x = if n = 0 then x else iter (n - 1) f (f x)

let sorted_squares : square list -> square list = List.sort Pervasives.compare
let board_squares = List.concat
