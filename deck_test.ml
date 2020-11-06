open OUnit2
open Deck
open Poker

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

let rec string_of_card_list = function
  | [] -> ""
  | h :: t -> 
    string_of_card h ^ ", " ^ string_of_card_list t

let string_of_hand = function
  | HighCard c -> "HighCard " ^ string_of_card_list c
  | Pair (r, c) -> 
    "Pair of " ^ string_of_int r ^ " with " ^ string_of_card_list c
  | TwoPairs (r1, r2, c) -> 
    "2 Pair of " ^ string_of_int r1 ^ ", " ^ string_of_int r2 ^ " with " ^ 
    string_of_card_list c
  | ThreeOfAKind (r, c) -> "3 of " ^ string_of_int r ^ " with " ^ 
                           string_of_card_list c
  | Straight c-> "straight" ^ string_of_card_list c
  | Flush c -> "flush " ^ string_of_card_list c
  | FullHouse (r1, r2) -> "full house, 3 of " ^ string_of_int r1 ^ " and 2 of "
                          ^ string_of_int r2
  | FourOfAKind (r, c) -> 
    "4 kind of " ^ string_of_int r ^ " with " ^ string_of_card_list c
  | StraightFlush c -> "straight flush " ^ string_of_card_list c
  | RoyalFlush -> "royal flush"

let test_hand_value name cards expected_output  =
  name >:: (fun _ -> 
      assert_equal expected_output (hand_value cards)
        ~printer: string_of_hand)

let test_cmp_hand name c1 c2 expected_output  =
  name >:: (fun _ -> 
      assert_equal expected_output (cmp_hand c1 c2)
        ~printer: string_of_int)

let p1  = make_card S 14
let p2  = make_card S 2
let p3  = make_card S 3
let p4  = make_card S 4
let p5  = make_card S 5
let p6  = make_card S 6
let p7  = make_card S 7
let p8  = make_card S 8
let p9  = make_card S 9
let p10 = make_card S 10
let p11 = make_card S 11
let p12 = make_card S 12
let p13 = make_card S 13
let p14 = make_card H 14
let p15 = make_card H 2
let p16 = make_card H 3
let p17 = make_card H 4
let p18 = make_card H 5
let p19 = make_card H 6
let p20 = make_card H 7
let p21 = make_card H 8
let p22 = make_card H 9
let p23 = make_card H 10
let p24 = make_card H 11
let p25 = make_card H 12
let p26 = make_card H 13
let p27 = make_card C 14
let p28 = make_card C 2
let p29 = make_card C 3
let p30 = make_card C 4
let p31 = make_card C 5
let p32 = make_card C 6
let p33 = make_card C 7
let p34 = make_card C 8
let p35 = make_card C 9
let p36 = make_card C 10
let p37 = make_card C 11
let p38 = make_card C 12
let p39 = make_card C 13
let p40 = make_card D 14
let p41 = make_card D 2
let p42 = make_card D 3
let p43 = make_card D 4
let p44 = make_card D 5
let p45 = make_card D 6
let p46 = make_card D 7
let p47 = make_card D 8
let p48 = make_card D 9
let p49 = make_card D 10
let p50 = make_card D 11
let p51 = make_card D 12
let p52 = make_card D 13

let l1 = [p3; p47; p12; p42; p28; p36; p41]
let l2 = [p16; p52; p17; p21; p14; p9; p46]
let l3 = [p4; p30; p6; p1; p32; p33; p43]
let l4 = [p50; p11; p31; p15; p8; p40; p26]
let l5 = [p45; p10; p29; p49; p5; p2; p19]
let l6 = [p49; p14; p8; p52; p48; p46; p38]
let l7 = [p31; p28; p20; p37; p34; p7; p4]
let l8 = [p16; p27; p43; p21; p3; p13; p26]
let l9 = [p22; p44; p45; p41; p7; p40; p50]
let l10 = [p39; p13; p32; p26; p1; p42; p38]
let l11 = [p1; p10; p12; p32; p50; p13; p11]
let l12 = [p9; p10; p12; p32; p50; p13; p11]
let l13 = [p48; p34; p38; p20; p49; p19; p31]
let l14 = [p22; p33; p48; p27; p35; p6; p9]

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

  test_hand_value "Two pairs" l1 (TwoPairs (3, 2, [p12]));
  test_hand_value "High Card" l2 (HighCard ([p14; p52; p9; p21; p46]));
  test_hand_value "Full house" l3 (FullHouse (4, 6));
  test_hand_value "Pair of J" l4 (Pair (11, [p40; p26; p8]));
  test_hand_value "Two pairs" l5 (TwoPairs (10, 6, [p5]));
  test_hand_value "High card" l6 (HighCard [p14; p52; p38; p49; p48]);
  test_hand_value "Pair of 7" l7 (Pair (7, [p37; p34; p31]));
  test_hand_value "Two pairs 3, K" l8 (TwoPairs (13, 3, [p27]));
  test_hand_value "flush" l9 (Flush ([p40; p50; p45; p44; p41]));
  test_hand_value "Three of a Kind (K)" l10 (ThreeOfAKind (13, [p1; p38]));
  test_hand_value "Royal Flush" l11 RoyalFlush;
  test_hand_value "Straight Flush" l12 (StraightFlush [p13; p12; p11; p10; p9]);
  test_hand_value "Straight" l13 (Straight [p49; p48; p34; p20; p19]);
  test_hand_value "Four of a Kind" l14 (FourOfAKind (9, [p27]));

  test_cmp_hand "Straight > Two pairs" l13 l1 1;
  test_cmp_hand "Flush < Royal Flush" l9 l11 (-1);
  test_cmp_hand "compare two high card, third card is different" l6 l2 1;
  test_cmp_hand "compare two same high card" l6 l6 0;
  test_cmp_hand "compare 2 two pairs, first pair is different" l1 l5 (-1);
  test_cmp_hand "compare pair, different pair" l4 l7 1;
]

let suite =
  "test suite for Deck"  >::: tests

let _ = run_test_tt_main suite