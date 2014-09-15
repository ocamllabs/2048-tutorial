(*
 * Copyright (c) 2014 Jeremy Yallop.
 *
 * This file is distributed under the terms of the BSD2 License.
 * See the file COPYING for details.
 *)

type stages =
    Winning_board
  | Shifting
  | Inserting
  | Game_over
  | Provenance

let tests_enabled = [
  Winning_board;
  (* Shifting; *)
  (* Inserting; *)
  (* Game_over; *)
  (* Provenance; *)
]

let test ?stage msg test =
  let open OUnit in
  match stage with
  | None -> msg >:: test
  | Some s when List.mem s tests_enabled -> msg >:: test
  | _ -> msg >:: fun () -> todo msg; assert false
