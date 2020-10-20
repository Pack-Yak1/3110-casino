open OUnit2
open Deck

let card_eq c1 c2 = 
  cmp_card c1 c2 = 0

type comparison = Greater | Lesser | Equal

let int_to_cmp x = 
  if x > 0 then Greater
  else if x < 0 then Lesser
  else Equal

let test_cmp_suit name c1 c2 expected_output = 
  name >:: (fun _ -> 
      assert_equal 
        expected_output (c2 |> cmp_suit c1 |> int_to_cmp))

let test_cmp_rank name c1 c2 expected_output = 
  name >:: (fun _ -> 
      assert_equal 
        expected_output (c2 |> cmp_rank c1 |> int_to_cmp))

let test_cmp_card name c1 c2 expected_output = 
  name >:: (fun _ -> 
      assert_equal 
        expected_output (c2 |> cmp_card c1 |> int_to_cmp))

let test_length name deck expected_output = 
  name >:: (fun _ -> 
      assert_equal 
        expected_output (length deck))

let test_sort name deck expected_output = 
  name >:: (fun _ -> 
      assert_equal 
        expected_output (sort deck))

let test_pick name deck n expected_output = 
  name >:: (fun _ -> 
      assert_equal 
        expected_output (pick deck n))

let test_bj_score name deck expected_output = 
  name >:: (fun _ -> 
      assert_equal 
        expected_output (bj_score deck))

let c1 = make_card S 2
let c2 = make_card H 3
let c3 = make_card C 4
let c4 = make_card D 5
let c5 = make_card S 11
let c6 = make_card H 12
let c7 = make_card C 13
let c8 = make_card D 14
let c9 = make_card S 14
let d0 = std_deck ()
let d1 = make_deck [c1; c2]
let d2 = make_deck [c3; c4]
let d3 = make_deck [c4; c3; c2; c1]
let d4 = make_deck [c4; c4; c4; c3; c3]
let d3_sorted = make_deck [c1; c2; c3; c4]
let d5 = make_deck []
let d6 = make_deck [c5; c6; c7]

let tests = [
  test_cmp_suit "spades > hearts" c1 c2 Greater;
  test_cmp_suit "hearts > clubs" c2 c3 Greater;
  test_cmp_suit "clubs > diamonds" c3 c4 Greater;
  test_cmp_suit "diamonds < clubs" c4 c3 Lesser;
  test_cmp_suit "hearts = hearts" c2 c2 Equal;

  test_cmp_rank "A > K" c8 c7 Greater;
  test_cmp_rank "K > Q" c7 c6 Greater;
  test_cmp_rank "Q > J" c6 c5 Greater;
  test_cmp_rank "5 < J" c4 c5 Lesser;
  test_cmp_rank "2 = 2" c1 c1 Equal;

  test_cmp_card "rank and suit greater" c5 c2 Greater;
  test_cmp_card "rank greater suit less" c7 c2 Greater;
  test_cmp_card "rank less suit greater" c1 c8 Lesser;
  test_cmp_card "rank less suit less" c4 c5 Lesser;
  test_cmp_card "rank equal suit greater" c9 c8 Greater;
  test_cmp_card "rank lesser suit equal" c4 c8 Lesser;
  test_cmp_card "equal rank and suit" c9 c9 Equal;

  test_length "zero" d5 0;
  test_length "two" d2 2;
  test_length "52" d0 52;

  test_sort "empty" d5 d5;
  test_sort "reversed" d3 d3_sorted;

  test_pick "empty" d5 2 None;
  test_pick "1st card" d3 0 (Some c4);
  test_pick "last card" d3 3 (Some c1);
  test_pick "only card" (make_deck [c3]) 0 (Some c3);

  test_bj_score "empty" d5 0;
  test_bj_score "4 and 5 = 9" d2 9;
  test_bj_score "face cards all 10" d6 30;
  test_bj_score "ace = 11" (make_deck [c1; c2; c9]) 16;
  test_bj_score "ace = 11 and 1" (make_deck [c1; c2; c9; c9]) 17;
  test_bj_score "ace makes 21" (make_deck [c9; c4; c4]) 21;
  test_bj_score "ace calculated last" (make_deck [c9; c7; c7]) 21;
]

let suite =
  "test suite for Deck"  >::: tests

let _ = run_test_tt_main suite