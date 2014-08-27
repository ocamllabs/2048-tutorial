open OUnit
open G2048
open Board_utils

let mk_board_test = QCheck.mk_test ~n:1000 ~pp:string_of_board

let check_board_property name ?size (prop : board -> bool) =
  assert QCheck.(run (mk_board_test (arbitrary_board ?size) prop))

let check_full_board_property name ?size (prop : board -> bool) =
  assert QCheck.(run (mk_board_test (arbitrary_full_board ?size) prop))

let test_shift_board_fixpoint () =
  check_board_property "Shifting reaches a fixpoint after width(board) shifts"
    (fun board ->
      let fixed = iter (List.length board) (shift_board L) board in
      board_equal (shift_board L fixed) fixed)

let test_add_to_full () =
  check_full_board_property "Squares cannot be added to a fully-populated board"
    (fun board -> insert_square t2 board = None)

let test_add () =
  check_board_property "Squares cannot be added to a fully-populated board"
    QCheck.(Prop.((fun board -> not (is_board_full board))
                  ==>
                  (fun board ->
                    insert_square t2 board <> None)))

(* Some tests for is_board_full *)
let test_is_board_full () =
  begin
    check_full_board_property "Randomly generated full boards are full"
      is_board_full;

    assert_equal true
      (is_board_full []);

    assert_equal true
      (is_board_full [[t2]]);

    assert_equal true
      (is_board_full [[t2; t4 ];
                      [t8; t16]]);

    assert_equal false
      (is_board_full [[empty]]);

    assert_equal false
      (is_board_full [[t2; empty];
                      [t4; t8   ]]);

    assert_equal false
      (is_board_full [[empty; empty];
                      [empty; empty]]);
  end

(* Some tests for is_board_winning *)
let test_is_board_winning () =
  begin
    assert_equal false
      (is_board_winning []);

    assert_equal true
      (is_board_winning [[t2048]]);

    assert_equal true
      (is_board_winning (shift_board L [[t2048]]));

    assert_equal false
      (is_board_winning [[t2; t4 ];
                         [t8; t16]]);

    assert_equal true
      (is_board_winning [[t2048; t2 ];
                         [t8   ; t16]]);

    assert_equal true
      (is_board_winning [[t2048; empty];
                         [t8   ; t16  ]]);

    assert_equal true
      (is_board_winning (shift_board L [[t2048; empty];
                                        [t8   ; t16  ]]));

    assert_equal false
      (is_board_winning [[empty]]);

    assert_equal false
      (is_board_winning [[empty; empty];
                         [empty; empty]]);
  end

(* Tests for insert_into_board *)
let test_insert () =
  let insert_property square board =
    let ofSome = function Some x -> x | None -> assert false in
   (* rely on the fact that `sort_squares` places empties first *)
    assert (not (is_board_full board));
    (sorted_squares (board_squares (ofSome (insert_square square board)))
     =
     sorted_squares (square :: List.tl (sorted_squares (board_squares board))))
  in
  check_board_property "insert_into_board adds a square to the board"
    QCheck.(Prop.((fun board -> not (is_board_full board))
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
    assert_equal (shift_board L board)
      ~cmp:board_equal
      ~printer:string_of_board
      [[t4   ; t4   ; empty; empty];
       [t2   ; t4   ; empty; empty];
       [empty; empty; empty; empty];
       [t16  ; empty; empty; empty]];

    assert_equal (shift_board R board)
      ~cmp:board_equal
      ~printer:string_of_board
      [[empty; empty; t4   ; t4   ];
       [empty; empty; t2   ; t4   ];
       [empty; empty; empty; empty];
       [empty; empty; empty; t16  ]];

    assert_equal (shift_board U board)
      ~cmp:board_equal
      ~printer:string_of_board
      [[t4   ; empty; t2   ; t8   ];
       [empty; empty; t8   ; t8   ];
       [empty; empty; empty; empty];
       [empty; empty; empty; empty]];

    assert_equal (shift_board D board)
      ~cmp:board_equal
      ~printer:string_of_board
      [[empty; empty; empty; empty];
       [empty; empty; empty; empty];
       [empty; empty; t2   ; t8   ];
       [t4   ; empty; t8   ; t8   ]];

    assert_equal (shift_board L (shift_board L board))
      ~cmp:board_equal
      ~printer:string_of_board
      [[t8   ; empty; empty; empty];
       [t2   ; t4   ; empty; empty];
       [empty; empty; empty; empty];
       [t16  ; empty; empty; empty]];

    assert_equal (shift_board U (shift_board U board))
      ~cmp:board_equal
      ~printer:string_of_board
      [[t4   ; empty; t2   ; t16  ];
       [empty; empty; t8   ; empty];
       [empty; empty; empty; empty];
       [empty; empty; empty; empty]];
  end

(* Some tests for provenance *)
let test_provenance () =
  let board = [[t2   ; empty; t2   ; t4   ];
               [t2   ; empty; empty; t4   ];
               [empty; empty; empty; empty];
               [empty; empty; t8   ; t8   ]]
  in
  begin
    let false_board = [[false; false; false; false];
                       [false; false; false; false];
                       [false; false; false; false];
                       [false; false; false; false]] in

    assert_equal (board_news (shift_board L board)) false_board;
    assert_equal (board_news (shift_board R board)) false_board;
    assert_equal (board_news (shift_board U board)) false_board;
    assert_equal (board_news (shift_board D board)) false_board;

    assert_equal (board_prevs (shift_board L board))
      [[Some 2 ; Some 4; None; None] ;
       [Some 2 ; Some 4; None; None] ;
       [None   ; None  ; None; None] ;
       [Some 8 ; None  ; None; None]];

    assert_equal (board_prevs (shift_board R board))
      [[None; None; Some 2; Some 4] ;
       [None; None; Some 2; Some 4] ;
       [None; None; None  ; None  ] ;
       [None; None; None  ; Some 8]];

    assert_equal (board_prevs (shift_board U board))
      [[Some 2; None; Some 2; Some 4];
       [None  ; None; Some 8; Some 8];
       [None  ; None; None  ; None  ];
       [None  ; None; None  ; None  ]];

    assert_equal (board_prevs (shift_board D board))
      [[None  ; None; None  ; None  ];
       [None  ; None; None  ; None  ];
       [None  ; None; Some 2; Some 4];
       [Some 2; None; Some 8; Some 8]];

    assert_equal (board_shifts (shift_board L board))
      [[Some 0 ; Some 2; None; None] ;
       [Some 0 ; Some 2; None; None] ;
       [None   ; None  ; None; None] ;
       [Some 2 ; None  ; None; None]];

    assert_equal (board_shifts (shift_board R board))
      [[None; None; Some 0; Some 0] ;
       [None; None; Some 2; Some 0] ;
       [None; None; None  ; None  ] ;
       [None; None; None  ; Some 0]];

    assert_equal (board_shifts (shift_board U board))
      [[Some 0; None; Some 0; Some 0] ;
       [None  ; None; Some 2; Some 2] ;
       [None  ; None; None  ; None  ] ;
       [None  ; None; None  ; None  ]];

    assert_equal (board_shifts (shift_board D board))
      [[None  ; None; None  ; None  ] ;
       [None  ; None; None  ; None  ] ;
       [None  ; None; Some 2; Some 1] ;
       [Some 2; None; Some 0; Some 0]];
  end

(* Some tests for scoring *)
let test_scoring () =
  begin
    assert_equal 0
     (last_move_score
        (shift_board L
           [[empty; t2  ; t8   ];
            [empty; t4  ; t16  ];
            [t1024; t512; t1024]]));

    assert_equal (2*2 + 2*16 + 2*512)
     (last_move_score
        (shift_board L
           [[t2   ; t2   ; t8  ];
            [empty; t16  ; t16 ];
            [t512 ; empty; t512]]));
  end


let suite = "2048 tests" >:::
  ["a fixpoint is reached after width(board) shift_boards"
    >:: test_shift_board_fixpoint;

   "squares can be added to a board that is not fully-populated"
    >:: test_add;

   "squares cannot be added to a fully-populated board"
    >:: test_add_to_full;

   "test is_board_full"
    >:: test_is_board_full;

   "test insert_into_board"
    >:: test_insert;

   "test movements"
    >:: test_movements;

   "test provenance"
    >:: test_provenance;

   "test is_board_winning"
    >:: test_is_board_winning;

   "test scoring"
    >:: test_scoring;
  ]
let _ =
  run_test_tt_main suite
