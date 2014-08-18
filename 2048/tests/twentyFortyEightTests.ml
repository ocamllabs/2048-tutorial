open OUnit
open TwentyFortyEight
open Board_utils

let check_board_property name ?size (prop : board -> bool) =
  assert QCheck.(run (mk_test ~name ~pp:string_of_board
                        (arbitrary_board ?size) prop))

let check_full_board_property name ?size (prop : board -> bool) =
  assert QCheck.(run (mk_test ~name ~pp:string_of_board
                        (arbitrary_full_board ?size) prop))

let test_shift_fixpoint () =
  check_board_property "Shifting reaches a fixpoint after width(board) shifts"
    (fun board -> 
      let fixed = iter (List.length board) (shift L) board in
      shift L fixed = fixed)
    
let test_add_to_full () =
  check_full_board_property "Tiles cannot be added to a fully-populated board"
    (fun board -> insert_into_board t2 board = None)

let test_add () =
  check_board_property "Tiles cannot be added to a fully-populated board"
    QCheck.(Prop.((fun board -> not (is_full_board board))
                  ==>
                  (fun board ->
                    insert_into_board t2 board <> None)))

let suite = "2048 tests" >:::
  ["a fixpoint is reached after width(board) shifts"
    >:: test_shift_fixpoint;

   "tiles can be added to a board that is not fully-populated"
    >:: test_add;

   "tiles cannot be added to a fully-populated board"
    >:: test_add_to_full;
  ]
let _ =
  run_test_tt_main suite
