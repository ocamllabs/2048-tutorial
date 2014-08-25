open OUnit
open G2048
open Board_utils

let mk_board_test = QCheck.mk_test ~n:1000 ~pp:string_of_board

let check_board_property name ?size (prop : board -> bool) =
  assert QCheck.(run (mk_board_test (arbitrary_board ?size) prop))

let check_full_board_property name ?size (prop : board -> bool) =
  assert QCheck.(run (mk_board_test (arbitrary_full_board ?size) prop))

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

(* Some tests for is_full_board *)
let test_is_full_board () =
  begin
    check_full_board_property "Randomly generated full boards are full"
      is_full_board;

    assert_equal true
      (is_full_board []);

    assert_equal true
      (is_full_board [[t2]]);

    assert_equal true
      (is_full_board [[t2; t4 ];
                      [t8; t16]]);

    assert_equal false
      (is_full_board [[empty]]);

    assert_equal false
      (is_full_board [[t2; empty];
                      [t4; t8   ]]);

    assert_equal false
      (is_full_board [[empty; empty];
                      [empty; empty]]);
  end

(* Tests for insert_into_board *)
let test_insert () =
  let insert_property tile board =
    let ofSome = function Some x -> x | None -> assert false in
   (* rely on the fact that `sort_tiles` places empties first *)
    assert (not (is_full_board board));
    (sorted_tiles (board_tiles (ofSome (insert_into_board tile board)))
     =
     sorted_tiles (tile :: List.tl (sorted_tiles (board_tiles board))))
  in
  check_board_property "insert_into_board adds a tile to the board"
    QCheck.(Prop.((fun board -> not (is_full_board board))
                     ==>
                  (insert_property t8)))



(* Some tests for movements *)
let test_movements () =
  let board = [[t2   ; empty; t2   ; t4   ];
               [t2   ; empty; empty; t4   ];
               [empty; empty; empty; empty];
               [empty; empty; t8   ; t8   ]]
  in
  begin
    assert_equal (shift L board)
      ~printer:string_of_board
      [[t4   ; t4   ; empty; empty];
       [t2   ; t4   ; empty; empty];
       [empty; empty; empty; empty];
       [t16  ; empty; empty; empty]];

    assert_equal (shift R board)
      ~printer:string_of_board
      [[empty; empty; t4   ; t4   ];
       [empty; empty; t2   ; t4   ];
       [empty; empty; empty; empty];
       [empty; empty; empty; t16  ]];

    assert_equal (shift U board)
      ~printer:string_of_board
      [[t4   ; empty; t2   ; t8   ];
       [empty; empty; t8   ; t8   ];
       [empty; empty; empty; empty];
       [empty; empty; empty; empty]];

    assert_equal (shift D board)
      ~printer:string_of_board
      [[empty; empty; empty; empty];
       [empty; empty; empty; empty];
       [empty; empty; t2   ; t8   ];
       [t4   ; empty; t8   ; t8   ]];

    assert_equal (shift L (shift L board))
      ~printer:string_of_board
      [[t8   ; empty; empty; empty];
       [t2   ; t4   ; empty; empty];
       [empty; empty; empty; empty];
       [t16  ; empty; empty; empty]];

    assert_equal (shift U (shift U board))
      ~printer:string_of_board
      [[t4   ; empty; t2   ; t16  ];
       [empty; empty; t8   ; empty];
       [empty; empty; empty; empty];
       [empty; empty; empty; empty]];
  end

let suite = "2048 tests" >:::
  ["a fixpoint is reached after width(board) shifts"
    >:: test_shift_fixpoint;

   "tiles can be added to a board that is not fully-populated"
    >:: test_add;

   "tiles cannot be added to a fully-populated board"
    >:: test_add_to_full;

   "test is_full_board"
    >:: test_is_full_board;

   "test insert_into_board"
    >:: test_insert;

   "test movements"
    >:: test_movements;
  ]
let _ =
  run_test_tt_main suite
