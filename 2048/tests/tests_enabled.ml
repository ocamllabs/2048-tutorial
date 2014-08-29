type stages =
    Winning_board
  | Shifting
  | Inserting
  | Game_over
  | Provenance

let tests_enabled = [
  Winning_board;
  Shifting;
  Inserting;
  (* Game_over; *)
  (* Provenance; *)
]

let test ?stage msg test =
  let open OUnit in
  match stage with
  | None -> msg >:: test
  | Some s when List.mem s tests_enabled -> msg >:: test
  | _ -> msg >:: fun () -> todo msg; assert false
