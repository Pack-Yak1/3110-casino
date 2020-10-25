open OUnit2
open Deck

let card_eq c1 c2 = 
  cmp_card c1 c2 = 0

type comparison = Greater | Lesser | Equal

let int_to_cmp x = 
  if x > 0 then Greater
  else if x < 0 then Lesser
  else Equal

(** [cmp_lists lst1 lst2] adopted from A2. *)
let cmp_lists lst1 lst2 =
  let uniq1 = List.sort compare lst1 in
  let uniq2 = List.sort compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

let test_cmp_suit name c1 c2 expected_output = 
  name >:: (fun _ -> 
      assert_equal expected_output (c2 |> cmp_suit c1 |> int_to_cmp))

let test_cmp_rank name c1 c2 expected_output = 
  name >:: (fun _ -> 
      assert_equal expected_output (c2 |> cmp_rank c1 |> int_to_cmp))

let test_cmp_card name c1 c2 expected_output = 
  name >:: (fun _ -> 
      assert_equal expected_output (c2 |> cmp_card c1 |> int_to_cmp))

let test_length name deck expected_output = 
  name >:: (fun _ -> 
      assert_equal expected_output (length deck))

let test_sort name deck expected_output = 
  name >:: (fun _ -> 
      assert_equal expected_output (sort deck))

let test_pick name deck n expected_output = 
  name >:: (fun _ -> 
      assert_equal expected_output (pick deck n))

let test_bj_score name deck expected_output = 
  name >:: (fun _ -> 
      assert_equal expected_output (bj_score deck) ~printer:string_of_int) 

let test_std_deck name expected_output =
  name >:: (fun _ -> 
      assert_equal expected_output (of_deck(std_deck())))

let test_return name cards n expected_output =
  name >:: (fun _ -> 
      assert_equal expected_output (of_deck (return cards n)))

let test_concat name d1 d2 expected_output =
  name >:: (fun _ -> 
      assert_equal expected_output (of_deck (concat d1 d2)))

let test_stable_concat name d1 d2 expected_output =
  name >:: (fun _ -> 
      assert_equal expected_output (of_deck (stable_concat d1 d2)))

let test_n_std_decks name n expected_output =
  name >:: (fun _ -> 
      assert_equal expected_output (length (n_std_decks n)) 
        ~printer:string_of_int)

let test_shuffle name lst expected_output  =
  name >:: (fun _ -> 
      assert_equal expected_output ~cmp:cmp_lists
        (of_deck (shuffle lst)))

let c1 = make_card S 2
let c2 = make_card H 3
let c3 = make_card C 4
let c4 = make_card D 5
let c5 = make_card S 11
let c6 = make_card H 12
let c7 = make_card C 13
let c8 = make_card D 14
let c9 = make_card S 14
let c10 = make_card D 2
let c11 = make_card D 3
let c12 = make_card D 4
let c13 = make_card D 6
let d0 = std_deck ()
let d1 = make_deck [c1; c2]
let d2 = make_deck [c3; c4]
let d3 = make_deck [c4; c3; c2; c1]
let d4 = make_deck [c4; c4; c4; c3; c3]
let d3_sorted = make_deck [c1; c2; c3; c4]
let d5 = make_deck []
let d6 = make_deck [c5; c6; c7]
let d7 = make_deck [c1]
let d8 = n_std_decks 3
let d9 = n_std_decks 1
let e1 = make_deck [c10; c11]
let e2 = make_deck [c10; c11; c12; c4; c13]
let e3 = make_deck [c4]
let e4 = make_deck [c4; c4; c4]
let e5 = n_std_decks 2
let e6 = make_deck [c5; c6; c7]
let e7 = make_deck [c2; c1; c3; c4]
let e8 = make_deck [c1; c2; c3; c4]
let e9 = make_deck [c4; c3; c2; c1;c4; c4; c4; c3; c3]
let e10 = make_deck [c7; c6; c5]
let s1 = shuffle (std_deck())
let s2 = shuffle (std_deck())
let s3 = shuffle d4
let s4 = shuffle d8

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
  test_length "one" d7 1;
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
  test_bj_score "natural blackjack" (make_deck [c5; c9]) ~-1;

  test_std_deck "test standard deck" (of_deck d9);

  test_return "first two cards of std deck" d0 2 (of_deck e1);
  test_return "first five cards of std deck" d0 5 (of_deck e2);
  test_return "first card of d3" d3 1 (of_deck e3);
  test_return "first three of d4" d4 3 (of_deck e4);

  test_concat "two standard decks" d0 d0 (of_deck e5);
  test_concat "empty deck + nonempty" d5 d6 (of_deck e6);
  test_concat "nonempty deck + empty" d6 d5 (of_deck e10);
  test_concat "concat two nonempty" d1 d2 (of_deck e7);

  test_stable_concat "empty deck + nonempty" d5 d6 (of_deck e6);
  test_stable_concat "nonempty deck + empty" d6 d5 (of_deck e6);
  test_stable_concat "concat two nonempty" d1 d2 (of_deck e8);
  test_stable_concat "concat two decks with same element" d3 d4 (of_deck e9);

  test_n_std_decks "one standard deck" 1 52;
  test_n_std_decks "two standard deck" 2 104;
  test_n_std_decks "five standard deck" 5 260;

  test_shuffle "shuffle standard deck" s1 (of_deck d0);
  test_shuffle "shuffle standard deck2" s2 (of_deck d0);
  test_shuffle "shuffle 3 standard deck" s2 (of_deck d0);
  test_shuffle "shuffle d4" s4 (of_deck d8);
]

let suite =
  "test suite for Deck"  >::: tests

let _ = run_test_tt_main suite