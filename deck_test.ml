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

let c1 = make_card S 2
let c2 = make_card H 5
let c3 = make_card C 12
let c4 = make_card D 14
let d0 = std_deck ()
let d1 = make_deck [c1; c2]
let d2 = make_deck [c3; c4]
let d3 = make_deck [c1; c2; c3; c4]
let d4 = make_deck [c4; c4; c4; c3; c3]

let tests = [
  test_cmp_suit "spades > hearts" c1 c2 Greater;
  test_cmp_suit "hearts > clubs" c2 c3 Greater;
  test_cmp_suit "clubs > diamonds" c3 c4 Greater;
  test_cmp_suit "diamonds < clubs" c4 c3 Lesser;
  test_cmp_suit "hearts = hearts" c2 c2 Equal;
]

let suite =
  "test suite for Deck"  >::: tests

let _ = run_test_tt_main suite