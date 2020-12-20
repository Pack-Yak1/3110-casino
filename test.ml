(*
Tools:
We could not unit test the Tools modules because the exposed functions
involve printing, modifying files, and changing player settings. As a result,
we playtested each function throughout different points of gameplay and in
different gameplay scenarios.

Main, Gamestate:
We could not unit test these modules because their exposed functions
are mainly game engines. Thus, we playtested each game.
Blackjack game: We playtested to see if each blackjack command was performed 
correctly. We checked to see if hit gave a new card to the player,
double did double the bet the player placed, and split created two separate
hand and calculated the result for each hand.
Poker game: Similar to Blackjack, we playtested to see each that each Poker
command was performed appropriately. We used glass-box testing to check each
type of game scenario regarding possible paths of gameplay, such as the first
person calling, everyone folding, invalid raises, the big blind being able to
check in pre-flop only, only 1 or 2 players, etc.
Baccarat game: We playtested to see if it follows the right Baccarat rule, 
returns the correct winning hand and individual winning status of each player at 
the end of each round. 
In general, we playtest to see if the engine prompted to ask player
to reenter information if input is invalid, cards are in random order for each 
game, switching between games at the end of play. Throughout, we
tried different scenarios to check that the screen was cleared correctly.

Player:
Playtested with Main, Gamestate.

Command, Input:
Similarly, we could not unit test these modules because they take and parse
user input. We playtested by testing many inputs, using a glass-box
approach to reach every possible branch.

Deck, Blackjack, Poker:
We used OUnit to test these modules. We developed test cases for each funtion in 
these three modules using glass-box testing. In addition, we used bisect to
check for and increase coverage. We turned off bisect for precondition violation,
and situation that will not happen in real game. Getters are not tested thorugh
OUnit, but instead playtested. 

Baccarat:
Playtested with Main, Gamestate. Make sure correct drawing rule (banker's rule,
player's rule) is followed in different score, and correct winner is determined
at the end of the game.
*)


open OUnit2
open Deck
open Blackjack
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

let test_deck_eq name d1 d2 expected_output = 
  name >:: (fun _ -> 
      assert_equal expected_output (deck_eq d1 d2) ~printer:string_of_bool)

let test_shuffle name lst expected_output  =
  name >:: (fun _ -> 
      assert_equal expected_output ~cmp:cmp_lists
        (of_deck (shuffle lst)))

let test_is_empty name lst expected_output  =
  name >:: (fun _ -> 
      assert_equal expected_output (is_empty lst))

let test_cmp_high_cards name c1 c2 expected_output = 
  name >:: (fun _ -> 
      assert_equal expected_output (c2 |> cmp_high_cards c1 |> int_to_cmp))

let test_rev_sort name deck expected_output = 
  name >:: (fun _ -> 
      assert_equal expected_output (rev_sort deck))

let string_of_rank_op = function
  | None -> "None"
  | Some r -> string_of_int r

let test_rank_of_pair name deck expected_output = 
  name >:: (fun _ -> 
      assert_equal expected_output (rank_of_pair deck) 
        ~printer: string_of_rank_op)

let test_rank_filter name rank deck expected_output = 
  name >:: (fun _ -> 
      assert_equal expected_output (rank_filter rank deck)
        ~printer: string_of_deck)

let test_flush name deck expected_output = 
  name >:: (fun _ -> 
      assert_equal expected_output (flush deck)
        ~printer: string_of_deck)

let test_straight name deck expected_output = 
  name >:: (fun _ -> 
      assert_equal expected_output (straight deck)
        ~printer: string_of_deck)

let test_straight_flush name deck expected_output = 
  name >:: (fun _ -> 
      assert_equal expected_output (straight_flush deck)
        ~printer: string_of_deck)

let test_ba_score name deck expected_output = 
  name >:: (fun _ -> 
      assert_equal expected_output (ba_score deck) ~printer:string_of_int)

let test_split name deck expected_output = 
  name >:: (fun _ -> 
      assert_equal expected_output (split deck))

let test_suit name card expected_output = 
  name >:: (fun _ -> 
      assert_equal expected_output (suit card))

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
let s5 = make_deck [c5; c5]

let p1  = make_card S 14
let p2  = make_card S 2
let p3  = make_card S 3
let p4  = make_card S 4
let p5  = make_card S 5
let p6  = make_card S 6
let p8  = make_card S 8
let p9  = make_card S 9
let p10 = make_card S 10
let p11 = make_card S 11
let p12 = make_card S 12
let p13 = make_card S 13
let p14 = make_card H 14
let p17 = make_card H 4
let p18 = make_card H 5
let p22 = make_card H 9
let p23 = make_card H 10
let p26 = make_card H 13
let p27 = make_card C 14
let p30 = make_card C 4
let p33 = make_card C 7
let p35 = make_card C 9
let p37 = make_card C 11
let p40 = make_card D 14
let p41 = make_card D 2
let p42 = make_card D 3
let p45 = make_card D 6
let p46 = make_card D 7
let p48 = make_card D 9
let p49 = make_card D 10

let d10 = make_deck [c8; c13]
let d11 = make_deck [c4; c13]
let d12 = make_deck [p9; p11]
let d13 = make_deck [p8; p9]
let d14 = make_deck [p41; p42]
let d15 = make_deck [p23; p30]
let d16 = make_deck [p5; p8; p9]
let d17 = make_deck [p26; p42; p1]
let d18 = make_deck [c7; p48]
let d19 = make_deck [p10; p11; p12; p13]
let d20 = make_deck [c8; c5]

(* S3, CJ, SQ, D3, S2, S10, D2 *)
let l1 = make_deck [p3; p37; p12; p42; p2; p10; p41]
let l1' = make_deck [p12]
let l1_rev = make_deck [p12; p37; p10; p42; p3; p41; p2]
let l1_filtered = make_deck [p3; p12; p42; p2; p10; p41]
let l1_no_3 = make_deck [p37; p12; p2; p10; p41]
(* S5, DA, HK, S2, H10, SJ, D6 *)
let l2 = make_deck [p5; p40; p26; p2; p23; p11; p45]
let l2' = make_deck [p40; p26; p11; p23; p45]
let l3 = make_deck [p23; p49; p33; p46; p10; p11; p45]
let l4 = make_deck [p30; p9; p46; p23; p11; p49; p45]
let l4' = make_deck [p11; p9; p46]
(* C4, S9, D7, H10, SJ, D10, C7*)
let l5 = make_deck [p30; p9; p46; p23; p11; p49; p33]
let l5' = make_deck [p11]
(* HK, S8, S9, CJ, S4, H5, S2 *)
let l6 = make_deck [p26; p8; p9; p37; p4; p18; p2]
let l6' = make_deck [p26; p37; p9; p8; p18]
let l7 = make_deck [p5; p40; p33; p26; p2; p46; p10]
let l7' = make_deck [p40; p26; p10]
let l8 = make_deck [p4; p26; p49; p26; p23; p3; p27]
let l8' = make_deck [p27]
let l9 = make_deck [p3; p30; p5; p10; p18; p9; p2]
let l9' = make_deck [p10; p9; p5; p3; p2]
let l10 = make_deck [p3; p10; p49; p30; p5; p23; p37]
let l10' = make_deck [p37; p5]
let l11 = make_deck [p1; p10; p12; p37; p42; p13; p11]
let l11' = make_deck [p1; p13; p12; p11; p10]
let l12 = make_deck [p3; p10; p12; p9; p42; p13; p11]
let l12' = make_deck [p13; p12; p11; p10; p9]
let l13 = make_deck [p3; p5; p45; p46; p11; p9; p30]
let l13' = make_deck [p46; p45; p5; p30; p3]
let l14 = make_deck [p22; p33; p48; p27; p35; p2; p9]
let l14' = make_deck [p27]
let l15 = make_deck [p1; p14; p40; p27; p35; p2; p9]
let l16 = make_deck [p1; p14; p40; p27; p13; p2; p8]
(* S3, CJ, CA, D3, S2, S10, D2 *)
let l17 = make_deck [p3; p37; p27; p42; p2; p10; p41]
let l18 = make_deck [p2; p1; p4; p5; p9; p10; p41]
let l18' = make_deck [p1; p10; p9; p5; p4; p2]
let l19 = make_deck [p1; p3; p4; p5; p8; p13; p12]
(* ace counts as 1 or 11 *)
let l20 = make_deck [c4; c5; c6; c7; c8; c9]
let l21 = make_deck [c1; c2; c3; c8; c9;]
let l22 = make_deck [c1; c2; c8; c8; c8]
let l23 = make_deck [c8; c8; c8; c8; c8; c8; c8; c8; c8; c8; c8]
let l24 = make_deck [c8; c8; c8; c8; c8; c8; c8; c8; c8; c8; c8; c8]
let l25 = make_deck [p1; p10; p12; p33; p48; p13; p11]
let l26 = make_deck [p14; p18; p22; p23; p26; p27; p2]
let l27 = make_deck [p2; p3; p4; p5; p45; p41; p22]
let l28 = make_deck [p2; p8; p9; p10; p11; p12; p1; p40]
let l28' = make_deck [p12; p11; p10; p9; p8]
let l29 = make_deck [p41; p42; p45; p46; p48; p49; p27]
let l29' = make_deck [p49; p48; p46; p45; p42; p41]
let l30 = make_deck [p41; p42; p45; p46; p48; p1; p27]
let l30' = make_deck [p48; p46; p45; p42; p41]
let l31 = make_deck [p14; p40; p48; p35; p9; p22; p23] 
let l31' = make_deck [p40]
let l32 = make_deck [p14; p40; p48; p35; p9; p18; p33]
let l33 = make_deck [p14; p40; p10; p23; p4; p30; p17]
let l34 = make_deck [p14; p40; p27; p48; p35; p9; p22]
let l35 = make_deck [p14; p40; p27; p35; p9; p22; p33]
let l36 = make_deck [p2; p3; p6; p11; p12; p17; p18]
let l36' = make_deck [p12; p11; p6; p3; p2]
let l37 = make_deck [p30; p9; p23; p11; p49; p4; p2]
let l37' = make_deck [p11]
let l38 = make_deck [p14; p40; p27; p4; p30; p22; p33]
let l39 = make_deck [p14; p40; p27; p4; p2; p22; p33]
let l40 = make_deck [p3; p10; p12; p9; p42; p8; p11]
let l41 = make_deck [p30; p2; p46; p23; p11; p49; p45]
let l42 = make_deck [p30; p9; p23; p12; p49; p4; p2]
let l43 = make_deck [p3; p10; p49; p30; p12; p23; p37]

let deck_tests = "Deck test suite" >::: [
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
    test_bj_score "aces count as 1 if over 21" l20 37;
    test_bj_score "aces count as 11 and 1 for total 21" l21 21;
    test_bj_score "aces count as 11 and for total under 21" l22 18;
    test_bj_score "11 aces count as 10x1, 1x11 for total 21" l23 21;
    test_bj_score "12 aces all count as 1" l24 12;
    test_bj_score "natural blackjack" d20 ~-1;

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

    test_deck_eq "empty deck = empty deck" d5 d5 true;
    test_deck_eq "different decks" d5 d3 false;
    test_deck_eq "shuffled deck are equal" s1 s2 true;

    test_shuffle "shuffle standard deck" s1 (of_deck d0);
    test_shuffle "shuffle standard deck2" s2 (of_deck d0);
    test_shuffle "shuffle 3 standard deck" s2 (of_deck d0);
    test_shuffle "shuffle d4" s4 (of_deck d8);

    test_is_empty "empty deck" d5 true;
    test_is_empty "non-empty deck" e10 false;
    test_is_empty "non-empty deck" l1 false;

    test_cmp_high_cards "two equal deck" l2 l2 Equal;
    test_cmp_high_cards "first card is different" l2 l6 Lesser;
    test_cmp_high_cards "third card is different" l17 l1 Greater;

    test_rev_sort "empty deck" d5 d5;
    test_rev_sort "nonempty deck" l1 l1_rev;

    test_rank_of_pair "no pair" l2 None;
    test_rank_of_pair "only one pair" (rev_sort l4) (Some 10);
    test_rank_of_pair "two pairs" (rev_sort l5) (Some 10);
    test_rank_of_pair "Four of a Kind" (rev_sort l14) (Some 9);
    test_rank_of_pair "empty deck" d5 None;

    test_rank_filter "empty deck" 0 d5 d5;
    test_rank_filter "nonexistent rank" 14 l1 l1;
    test_rank_filter "only one card has target rank" 11 l1 l1_filtered; 
    test_rank_filter "more than one card has target rank" 3 l1 l1_no_3; 

    test_flush "no flush" l1 d5;
    test_flush "5 cards flush" l9 l9';
    test_flush "6 cards flush" l18 l18';
    test_flush "7 cards flush" l19 (rev_sort l19);
    test_flush "6 cards flush" l29 l29';
    test_flush "5 cards flush" l30 l30';

    test_straight "no straight" l1 d5;
    test_straight "straight" l13 l13';
    test_straight "straight" l28 l28';

    test_straight_flush "no straight or flush" l1 d5;
    test_straight_flush "straight only" l13 d5;
    test_straight_flush "flush only" l9 d5;
    test_straight_flush "straight flush" l12 l12';
    test_straight_flush "royal flush" l11 l11';

    test_ba_score "Ace and 6 = 7" d10 7; 
    test_ba_score "5 and 6 = 1" d11 1;
    test_ba_score "9 and J = 9" d12 9;
    test_ba_score "8 and 9 = 7" d13 7;
    test_ba_score "2 and 3 = 5" d14 5;
    test_ba_score "10 and 4 = 4" d15 4;
    test_ba_score "5 and 8 and 9 = 2" d16 2;
    test_ba_score "Q and 3 and Ace = 4" d17 4;
    test_ba_score "empty deck" d5 0;
    test_ba_score "K and 9 = 9" d18 9;
    test_ba_score "10, J, Q, K = 0" d19 0;

    test_split "split [S2,H3]" d1 (make_deck [c1], make_deck [c2]);
    test_split "split [C4,D5]" d2 (make_deck [c3], make_deck [c4]);
    test_split "split [S11,S11]" s5 (make_deck [c5], make_deck [c5]);

    test_suit "S2" c1 S;
    test_suit "H3" c2 H;
    test_suit "C4" c3 C;
    test_suit "D5" c4 D;
  ]

(** [bj_win_check_test name outcome player_score exp] constructs an OUnit
    test with name [name] that asserts the equality of [exp] with
    [win_check outcome player_score]. *)
let bj_win_check_test name outcome player_score exp =
  name >:: (fun _ -> assert_equal exp (win_check outcome player_score))

let bj_tests = "Blackjack test suite" >::: [
    bj_win_check_test "DealerBust 20 true" DealerBust 20 true;
    bj_win_check_test "DealerBust 21 true" DealerBust 21 true;
    bj_win_check_test "DealerBust 22 false" DealerBust 22 false;
    bj_win_check_test "Dealer 21 20 false" (Match 21) 20 false;
    bj_win_check_test "Dealer 21 21 false" (Match 21) 21 false;
    bj_win_check_test "Dealer 21 22 false" (Match 21) 22 false;
    bj_win_check_test "Dealer 18 17 false" (Match 18) 18 false;
    bj_win_check_test "Dealer 18 18 false" (Match 18) 18 false;
    bj_win_check_test "Dealer 18 19 true" (Match 18) 19 true;
    bj_win_check_test "Dealer 18 21 true" (Match 18) 21 true;
    bj_win_check_test "Dealer 18 22 false" (Match 18) 22 false;
    bj_win_check_test "Dealer no BJ player BJ" (Match 21) ~-1 true;
  ]

let test_hand_value name deck expected_output  =
  name >:: (fun _ -> 
      assert_equal expected_output (hand_value deck)
        ~printer: string_of_hand)

let test_cmp_hand name c1 c2 expected_output  =
  name >:: (fun _ -> 
      assert_equal expected_output (c2 |> cmp_hand c1 |> int_to_cmp))

let poker_tests = "Poker test suite" >::: [
    test_hand_value "Two pairs" l1 (TwoPairs (3, 2, l1'));
    test_hand_value "High Card" l2 (HighCard l2');
    test_hand_value "Full house" l3 (FullHouse (10, 7));
    test_hand_value "Pair of 10" l4 (Pair (10, l4'));
    test_hand_value "Two pairs" l5 (TwoPairs (10, 7, l5'));
    test_hand_value "High card" l6 (HighCard l6');
    test_hand_value "Pair of 7" l7 (Pair (7, l7'));
    test_hand_value "Two pairs 10, K" l8 (TwoPairs (13, 10, l8'));
    test_hand_value "flush" l9 (Flush l9');
    test_hand_value "Three of a Kind (10)" l10 (ThreeOfAKind (10, l10'));
    test_hand_value "Royal Flush" l11 RoyalFlush;
    test_hand_value "Straight Flush" l12 (StraightFlush l12');
    test_hand_value "Straight" l13 (Straight l13');
    test_hand_value "Four of a Kind" l14 (FourOfAKind (9, l14'));
    test_hand_value "One pair, and Four of a Kind" l31 (FourOfAKind (9, l31'));
    test_hand_value "One pair > three of kinds" l32 (FullHouse (9, 14));
    test_hand_value "Two pair, three of kinds" l33 (FullHouse (4, 14));
    test_hand_value "three of kinds > Four of kinds" l34 (FourOfAKind (9, l14'));
    test_hand_value "2 three of kinds" l35 (FullHouse (14, 9));
    test_hand_value "straight, flush, not striahgt flush" l36 (Flush l36');
    test_hand_value "2 pair (10, 4)" l37 (TwoPairs (10, 4, l37'));

    test_cmp_hand "Straight > Two pairs" l13 l1 Greater;
    test_cmp_hand "Flush < Royal Flush" l9 l11 Lesser;
    test_cmp_hand "two high card" l6 l2 Lesser;
    test_cmp_hand "two same high card" l6 l6 Equal;
    test_cmp_hand "2 two pairs, first pair is different" l1 l5 Lesser;
    test_cmp_hand "compare pair, different pair" l4 l7 Greater;
    test_cmp_hand "two Four of a kind, different rank of 4" l14 l15 Lesser;
    test_cmp_hand "two Four of a kind, different single card" l15 l16 Lesser;
    test_cmp_hand "two royal flush" l11 l25 Equal;
    test_cmp_hand "two different flush" l9 l26 Lesser;
    test_cmp_hand "two different straight" l13 l27 Greater;
    test_cmp_hand "2 two pairs, second pair is different" l5 l37 Greater;
    test_cmp_hand "2 Full house, pair is different" l35 l38 Greater;
    test_cmp_hand "2 three of a kinds" l10 l39 Lesser;
    test_cmp_hand "two different straightflush" l12 l40 Greater;
    test_cmp_hand "two same pair" l4 l41 Greater;
    test_cmp_hand "two pairs with same two same rank" l37 l42 Lesser;
    test_cmp_hand "2 three of a kinds with different high cards" l10 l43 Lesser;
  ]

let suite =
  "test suite for Deck, Blackjack, and Poker"  >::: [
    deck_tests; 
    bj_tests;
    poker_tests;
  ]

let _ = run_test_tt_main suite