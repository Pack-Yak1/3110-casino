open Player
open Deck
open ANSITerminal
open Blackjack
open Command
open Tools
open Baccarat
open Input

type player = Player.t

type t = {
  name : string;
  mutable game_deck : Deck.t;
  mutable flop : player;
  mutable player_num : int;
  turn : int;
  mutable players : player list;
  currency : string;
  small_blind : int;
  num_decks : int;
  mutable ba_players : player list;
}

let default_currency = "USD"
let default_blind = 0
let blind_msg s_b = "Small blind: " ^ string_of_int s_b ^ "\nBig Blind: "
                    ^ string_of_int (2 * s_b)

let default_game = {
  name = "";
  game_deck = empty_deck ();
  flop = Player.flop;
  player_num = 0;
  turn = 0;
  players = [];
  currency = default_currency;
  small_blind = 0;
  num_decks = 0;
  ba_players = [];
}

(************* Begin prompts & messages *************)

let number_of_decks_msg = "Please enter the number of decks to play with. It \
                           must be a number greater than 0.\n"
let invalid_p_msg = "You have entered an invalid number of players. Please \
                     enter a number greater than 0.\n"
let invalid_n_msg = "You have entered an invalid number of decks. Please enter \
                     a number greater than 0.\n"
let number_of_players_msg = "Please enter the number of players.\n"
let blind_seln_msg = "Please enter the value of the small blind. The value of \
                      the big blind is twice the value of the small blind."
let invalid_blind_msg = "The small blind must be positive."
let bet_msg player_name =
  player_name ^  ", please enter how much you wish to bet.\n"
let invalid_bet_msg = "You have entered an invalid bet. Please enter a number \
                       greater than 0 and less than your limit.\n"
let bet_over_money_msg = "You do not have enough money to raise.\n"
let bet_over_rem_msg lim = "You do not have enough money to raise by this \
                            amount. You can bet up to " ^ lim ^ " more.\n"
let current_bet_msg player curr =
  "You currently have "^ string_of_int player.bet ^ " " ^ curr ^ " bet.\n"
let invalid_double_msg = "You do not have enough money to double."
let invalid_command_msg = "You have entered an invalid command. Please try \
                           again.\n"
let invalid_check_msg = "A player has already opened the betting round. You \
                         may no longer check.\n"
let copy_suffix = "(copy)"
let not_unique_msg = "You cannot double down or split because you have already\
                      split this round.\n"
let non_2_card_split_msg = "You can only split if you have exactly 2 cards.\n"
let unequal_split_msg = "You can only split if both your cards have the same \
                         value.\n"
let not_enough_to_split_msg = "You do not have enough money to split.\n"
(** [turn_msg n] is a string prompt for the [n-th] user to enter their
    command. *)
let turn_msg (state : t) n = 
  let name = (List.nth state.players n).name in
  "It is now " ^ name ^ "'s turn.\n"
let repeat_game_msg game_name = 
  "\nDo you wish to play another round of " ^ game_name ^ "? (y/n)"
let change_game_msg = "\nWould you like to play a different game then? (y/n)"
let final_score_header = "\nFinal Scores:\n"
let no_players_left_msg = "There are no players left in the game."
let elimination_msg name = 
  name ^ " is bankrupt and has been eliminated.\n"
let goodbye_msg = "Goodbye!"

(************* End prompts & messages *************)

(************* Begin display (PRINT) functions *************)

(** [print_hand state index] prints the hand of the player with 0-based index
    [index] in [state]. *)
let print_hand state index = 
  let player = List.nth state.players index in
  let prefix = "Your current hand is " in
  player.hand |> string_of_deck |> ( ^ ) prefix |> print_string player.style;
  print_string [] "\n"

(** [start_turn] prints a helpful message containing information about the 
    player's hand, as well as the flop cards, if it is a game of Texas Hold'em
    and there are flop cards on the table. *)
let start_turn state player = 
  if length state.flop.hand > 0 then begin
    let flop_msg = 
      "The community cards are: " ^ (state.flop.hand |> string_of_deck) in
    print_endline flop_msg;
  end else ();
  player |> turn_msg state |> print_endline;
  print_hand state player;
  print_string ((List.nth state.players player).style) Input.input_prompt

(** [display_final_scores state] prints the final scores of all players
    in [state]. *)
let display_final_scores state = 
  print_endline final_score_header;
  let lst = state.players in
  List.iter (Player.print_score state.currency) lst

(************* End display (PRINT) functions *************)

(************* Begin state initializer (EVAL) & helpers *************)

(** [start_deck state n] is a game state equivalent to [state], except with 
    the deck field updated to be a shuffled deck of [n] 52-card standard 
    decks. *)
let start_deck state n : t =
  let deck = n |> n_std_decks |> shuffle in { state with game_deck = deck }

(** [update_players n p s] is [s] with [player_num] equal to [n], and [players]
    equal to [p]. *)
let update_players n p s =
  { s with player_num = n; players = p }

(** [shared_init s init_bet has_dealer starting_cards turn] is a game state 
    with number of decks, number of players, player names and initial bets 
    (iff [init_bet] is true) taken from user responses to prompts. The state 
    initializes a dealer iff [has_dealer] is true, and deals each player 
    [starting_cards] cards. *)
let shared_init s init_bet has_dealer starting_cards = 

  (* Select number of decks for game *)
  let n = choose_num_geq_1_leq_n number_of_decks_msg invalid_n_msg 0 false in

  (* Select number of players *)
  let num_players =
    choose_num_geq_1_leq_n number_of_players_msg invalid_p_msg 0 false in

  (* Name each player *)
  let players = Player.create_players num_players in

  (* Create initial gamestate with values entered above *)
  let s_with_decks = { (start_deck s n) with num_decks = n } in
  let s_with_players = update_players num_players players s_with_decks in 
  {s_with_players with currency = update_currency default_currency}

(************* End state initializer (EVAL) & helpers *************)

(************* Begin query (EVAL) functions *************)

(** [remaining_players state] is a list of players still in the game in
    [state] *)
let remaining_players state =
  List.filter (fun p -> p.in_game = true) state.players

(** [remaining_players_n state] is the number of remaining players in the
    game in state [state]. *)
let remaining_players_n state =
  remaining_players state |> List.length

(** [all_maxed_bets state] is [true] if all remaining players have betted all
    their chips and [false] otherwise. If there are no players in [state],
    it is [true]. *)
let all_maxed_bets state =
  let rem_players = remaining_players state in
  let non_maxed_players = List.filter (fun p -> 
      p.bet < p.money) rem_players in
  List.length non_maxed_players = 0

(** [all_bets_matched_helper lst bet acc] is [true] if all the players in [lst]
    still in the game have a bet equal to [bet] and [acc] is true.
    Otherwise, it is [false]. *)
let rec all_bets_matched_helper lst bet acc = 
  match lst with
  | [] -> true
  | h :: t -> begin
      let bets_match = h.bet = bet && h.in_game in
      if bets_match
      then all_bets_matched_helper t bet (acc && bets_match)
      else false
    end

(** [all_bets_matched state] is [true] if all players have the same bet
    amount and [false] if not. If there are no players, it is [true]. *)
let all_bets_matched state = 
  match remaining_players state with
  | [] -> true
  | h :: t -> all_bets_matched_helper t h.bet true

(** [find_index_h x acc lst] is the index of [x] in [lst] if it is present,
    shifted up by [acc], the number of elements in the list of interest
    preceding [lst]. If not in [lst], behavior is unspecified. *)
let rec find_index_h x acc = function
  | [] -> raise Not_found
  | h :: t -> if h = x then acc else find_index_h x (acc + 1) t

(** [find_index x lst] is the index of [x] in [lst] if it is present.
    If not, behavior is unspecified. *)
let find_index x lst =
  find_index_h x 0 lst

(************* End query (EVAL) functions *************)

(************* Begin bet setting (EVAL) & helper functions *************)

(** [assign_single_bet player] is [player] with prompted bet amount 
    assigned. *)
let assign_single_bet (player : player) st = 
  let msg = bet_msg player.name in
  let bet =
    Input.choose_num_geq_1_leq_n msg invalid_bet_msg player.money true in
  let p = { player with bet = bet } in 
  if st.name = "baccarat" then 
    let b = Input.choose_bet () in {p with bet_on = b}
  else p

(** [assign_bets_helper lst n acc] is a list of all [n] players in [lst] with 
    a bet of prompted value assigned to each. *)
let rec assign_bets_helper lst n acc st =
  match lst with 
  | [] -> acc
  | p :: t -> 
    if n = 0 then acc else begin
      assign_bets_helper t (n - 1) (acc @ [assign_single_bet p st]) st
    end

(** [assign_bets n state] is [state] with a bet (amount prompted) assigned
    to all [n] players in [state]. *)
let assign_bets n state = 
  assign_bets_helper state.players n [] state

(************* End bet setting (EVAL) & helper functions *************)

(************* Begin dealing (EVAL) & helper functions *************)

(** [refill_deck state] is a shuffled standard 52-card deck. *)
let refill_deck state = 
  let top_up = std_deck () |> shuffle in
  { state with game_deck = top_up }

(** [deal player state] deals a card from the game deck in [state] to the hand
    of [player]. *)
let rec deal player state = 
  match pick state.game_deck 0 with
  | None -> refill_deck state |> deal player
  | Some card -> begin
      player.hand <- append player.hand card;
      state.game_deck <- remainder state.game_deck
    end

(** Applies [deal player state] [n] times. *)
let deal_n n state player =
  for i = 1 to n do
    deal player state
  done

(** Deals [n] cards to all players in [state], then [n] cards to the dealer,
    if [has_dealer]. *)
let deal_all state n has_dealer = 
  let players = state.players in
  List.iter (deal_n n state) players;
  if has_dealer then deal_n n state dealer else ()

(************* End dealing (EVAL) & helper functions *************)

(************ Begin result processing & payment (EVAL) functions ************)

(** [bj_showdown state] is a list containing whether the players in [state]
    have won or not, in their respective order *)
let bj_showdown state =
  let dealer_score = bj_score dealer.hand in
  let outcome = if dealer_score > 21 then DealerBust else Match dealer_score in
  let scores = List.map (fun p -> bj_score p.hand) state.players in
  List.map (win_check outcome) scores

(** [pay_player curr win win_msg loss_msg p] pays [p] with name [name]
    if they win, given by [win], or charges them if they do not win, and prints
    the result of whether they win/lose (with [win_msg] or [loss_msg],
    respectively) and their remaining money, in [state] *)
let pay_player curr win win_msg loss_msg name p =
  if win then begin
    p.money <- p.bet + p.money;
    name ^ " " ^ win_msg ^ " and has " ^ string_of_int p.money
    ^ " " ^ curr ^ " total." |> print_endline;
  end else begin
    p.money <- p.money - p.bet;
    name ^ " " ^ loss_msg ^ " and has "  ^ string_of_int p.money
    ^ " " ^ curr ^ " total."|> print_endline;
  end

(** [reset_bets s] is [s] with the bets of all players set to 0. *)
let reset_bets s =
  List.iter (fun p -> p.bet <- 0) s.players

(************ End result processing & payment (EVAL) functions ************)

(************* Begin game reset functions (EVAL) & helpers *************)

(** [wipe_hands state] is [state] with all players' hands set to the empty 
    deck. *)
let wipe_hands state = 
  List.iter (fun p -> p.hand <- Deck.empty_deck ()) state.players

(** [refresh_state state init_bet] is [state] with all players' and dealer's
    hands set to empty, decks shuffled, flop empty, overall stats updated.
    Initial bets are assigned if [init_bet]. *)
let refresh_state state init_bet = 
  dealer.hand <- empty_deck ();
  wipe_hands state;
  state.game_deck <- n_std_decks state.num_decks |> shuffle;
  state.flop.hand <- empty_deck ();
  List.iter (fun p -> p.in_game <- true) state.players;
  if init_bet then begin 
    let new_players = assign_bets state.player_num state in
    { state with turn = 0; players = new_players }
  end else { state with turn = 0 }

(** [eliminate_player player] is [true] if the player is not eliminated.
    If the player is eliminated, it is [false]. *)
let eliminate_player player =
  if player.money > 0 then true
  else begin
    player.name |> elimination_msg |> print_endline;
    false
  end

(** [eliminate_bankrupts state] is [state] with all bankrupt players removed 
    from [state.players]. Prints a message whenever a player is eliminated. *)
let eliminate_bankrupts state = 
  let lst = state.players in
  let new_players = List.filter eliminate_player lst in
  { state with players = new_players; player_num = List.length new_players}

(************* Begin game reset functions (EVAL) & helpers *************)

(************ Begin shared protocols ************)

(** [quit_protocol] ends the game instantly and prints a goodbye message.
    It is shared by all game modes. *)
let quit_protocol state = 
  print_endline goodbye_msg;
  exit 0

(** [invalid_protocol engine state] prompts for another input in the same
    state. *)
let invalid_protocol (engine : t -> t) (state : t) : t = 
  print_endline invalid_command_msg;
  engine state 

(************ End shared protocols ************)

(************* Begin blackjack engine (REPL) & helpers *************)

(** [bj_payout state win_msg loss_msg player_outcomes] pays out each player in
    [state] who wins with [win_msg] and charges each player who loses with
    [loss_msg], whose victory or lack thereof is given by
    the respective element of [player_outcomes]. It also prints the money for
    each player and updates statistics. *)
let bj_payout state win_msg loss_msg player_outcomes =
  let players = state.players in
  for i = 0 to state.player_num - 1 do
    let player = List.nth players i in
    let win = List.nth player_outcomes i in
    pay_player state.currency win win_msg loss_msg player.name player;
    update_player_stats player.name
      (string_of_int player.money ^ " " ^ state.currency) state.name win
  done; reset_bets state; state

(** Plays the dealer's turn, draws until deck score exceeds 17 *)
let bj_dealer_turn state = 
  while bj_score dealer.hand < 17 do
    deal dealer state;
  done;
  print_string [] "Dealer's hand is ";
  dealer.hand |> Deck.string_of_deck |> print_endline;
  bj_showdown state |> bj_payout state
    "won against the dealer" "lost against the dealer"

(** Prompts player to enter a command for their turn in Blackjack. Returns a 
    [game_state] with the results of the blackjack turn applied.*)
let rec bj_turn s =
  let active_player_index = s.turn in
  if active_player_index >= s.player_num then bj_dealer_turn s
  else
    let active_player = List.nth s.players active_player_index in
    start_turn s active_player_index;
    try match read_line() |> parse_bj with
      | Hit -> bj_hit_protocol active_player s 
      | Stand -> bj_stand_protocol active_player s 
      | Double -> bj_double_protocol active_player s
      | Split -> bj_split_protocol active_player s
      | Quit -> quit_protocol s
      | Tools -> Tools.show_menu s.name active_player; bj_turn s
    with
    | Invalid_command -> invalid_protocol bj_turn s

and bj_split_protocol player state = 
  (* Checks that player has not split before *)
  if not_unique player state then begin
    print_endline not_unique_msg;
    bj_turn state
  end
  (* Checks that player has exactly 2 cards *)
  else if Deck.length player.hand <> 2 
  then begin
    print_endline non_2_card_split_msg;
    bj_turn state
  end
  else if player.bet > player.money / 2 then begin
    print_endline not_enough_to_split_msg;
    bj_turn state
  end else let hand = player.hand in
    (* Check cards have equal score in blackjack *)
    match pick hand 0, pick hand 1 with
    | Some c1, Some c2 -> begin
        let d1, d2 = make_deck [c1], make_deck [c2] in
        if bj_score d1 <> bj_score d2 
        then begin
          print_endline unequal_split_msg;
          bj_turn state
        end
        else begin
          (* Split is legal; begin to split and top up original player hand *)
          player.hand <- d1; 
          deal player state;
          (* Create split's hand and dummy player *)
          let name' = player.name ^ " " ^ copy_suffix in
          let copy = 
            {Player.default_player with name = name'; hand = d2; 
                                        bet = player.bet; money = 0} in
          (* Top up dummy player's hand *)
          deal copy state;
          (* Adds the dummy player to turn order and increments player_num *)
          insert_clone player state copy
        end
      end
    | _ -> failwith "impossible, player must have 2 cards"

(** [insert_clone player state copy] is [bj_turn] called on [state], with 
    [state.player_num] incremented by one, and [copy] inserted to 
    [state.players] after the occurance of [player]. 
    Requires: [player] is an element of [state.players] *)
and insert_clone player state copy = 
  state.player_num <- state.player_num + 1;
  let player_list = state.players in
  let player_list' = insert_after player player_list copy in
  let state' = {state with players = player_list'} in
  bj_turn state'

(** [insert_after p lst copy] is [copy] inserted to [lst] after the occurance
    of [p] in [lst].
    Requires: [p] is an element of [lst] *)
and insert_after p lst copy = 
  match lst with
  | [] -> raise Not_found
  | h :: t -> if h = p then h :: copy :: t else h :: insert_after p t copy

(** [is_copy] player is true if [player.name] ends with [copy_suffix], else
    it is false. *)
and is_copy (player : Player.t) =
  Player.ends_x copy_suffix player.name

(** [has_copy player state] is true if [state.players] contains a copy of
    [player].  *)
and has_copy (player : Player.t) state = 
  let check = player.name ^ " " ^ copy_suffix in
  List.filter (fun (p : Player.t) -> p.name = check) state.players
  |> List.length 
  |> ( <> ) 0

(** [not_unique player state] is true if [player] has a copy or is itself a 
    copy. *)
and not_unique player state = 
  has_copy player state || is_copy player

and bj_hit_protocol player state = 
  deal player state;
  bj_turn state

and bj_stand_protocol player state = 
  print_hand state state.turn;
  bj_turn { state with turn = state.turn + 1 }

and bj_double_protocol player state =
  (* Check that player has not split before. *)
  if not_unique player state then begin
    print_endline not_unique_msg;
    bj_turn state
  end
  (* Check player has sufficient money to double. *)
  else if player.bet <= player.money / 2 then begin
    player.bet <- player.bet * 2;
    deal player state;
    bj_stand_protocol player state
  end else begin
    print_endline invalid_double_msg;
    bj_turn state
  end

(************* End blackjack engine (REPL) & helpers *************)

(************* Begin baccarat engine (REPL) & helpers *************)

(** [banker_ba st] is the player in gamestate [st] in a Baccarat game.  
    Requires: ba_players in [st] contains exactly two players. *)
let player_ba st = 
  match st.ba_players with 
  | [] -> failwith "Wrong number of ba_players"
  | h :: t -> h

(** [banker_ba st] is the banker in gamestate [st] in a Baccarat game. 
    Requires: ba_players in [st] contains exactly two players. *)
let banker_ba st = 
  match st.ba_players with 
  | h :: h2 :: [] -> h2
  | _ -> failwith "Wrong number of ba_players"

(** [third st] is rank of third card received by player. 
    Requires: the banker's hand in [st] has exactly three cards. *)
let third st = 
  let c = return (player_ba st).hand 1 |> of_deck in
  match c with 
  | [n] -> rank n 
  | _ ->  failwith "Wrong number of cards"

let ba_helper st = 
  print_string [] "The Player's hand is ";
  (player_ba st).hand |> Deck.string_of_deck |> print_endline; 
  print_string [] "The Banker's hand is ";
  (banker_ba st).hand |> Deck.string_of_deck |> print_endline;
  let b_score = ba_score (banker_ba st).hand in
  let p_score = ba_score (player_ba st).hand in 
  if b_score > p_score then Banker
  else if  b_score < p_score then Player 
  else Tie

let banker_rule st = 
  let b_score = ba_score (banker_ba st).hand in
  let p_cards = length (player_ba st).hand in 
  if p_cards = 2 then begin 
    if b_score = 6 || b_score = 7 then ()
    else deal_n 1 st (banker_ba st);
  end else begin 
    let p = third st in 
    match b_score with 
    | 2 -> deal_n 1 st (banker_ba st)
    | 3 -> if p = 8 then () else deal_n 1 st (banker_ba st)
    | 4 -> if p = 2 || p = 3 || p = 4 || p = 5 || p = 6 || p = 7 
      then deal_n 1 st (banker_ba st) else ()
    | 5 -> if p = 4 || p = 5 || p = 6 || p = 7 
      then deal_n 1 st (banker_ba st) else ()
    | 6 -> if p = 6 || p = 7 
      then deal_n 1 st (banker_ba st) else ()
    | _ -> () end;
  ba_helper st 

let player_rule p st = 
  if p = 6 || p = 7 then ()
  else deal_n 1 st (player_ba st);
  banker_rule st

let ba_result st = 
  let b = ba_score (banker_ba st).hand in 
  let p = ba_score (player_ba st).hand in 
  match (p, b) with
  | 8, 8 -> Tie
  | 9, 9 -> Tie
  | 8, 9 -> Tie 
  | 9, 8 -> Tie
  | 8, _ -> Player
  | 9, _ -> Player
  | _, 8 -> Banker
  | _, 9 -> Banker
  | _ -> player_rule p st

let ba_showdown outcome st = 
  let players = st.players in 
  for i = 0 to st.player_num - 1 do 
    let player = List.nth players i in 
    let win = List.nth (List.map (fun x -> x.bet_on = outcome) players) i in 
    pay_player st.currency win "won" "lost" player.name player;
    update_player_stats player.name
      (string_of_int player.money ^ " " ^ st.currency) st.name win
  done; reset_bets st; st

(** Plays a game of baccarat and returns a game_state with the results of the
    game applieed. *)
let ba_turn s =
  let banker = {default_player with name = "banker"} in
  let player =  {default_player with name = "player"} in
  s.ba_players <- [player; banker];
  List.iter (deal_n 1 s) s.ba_players;
  List.iter (deal_n 1 s) s.ba_players;
  print_string [] "Player's hand is ";
  player.hand |> Deck.string_of_deck |> print_endline; 
  print_string [] "Banker's hand is ";
  banker.hand |> Deck.string_of_deck |> print_endline; 
  print_endline "Press anything to continue.\n"; 
  ignore (read_line());
  let outcome = ba_result s in begin
    match outcome with 
    | Player -> print_endline "Player won"
    | Banker -> print_endline "Banker won"
    | Tie -> print_endline "It's a tie"
  end;
  ba_showdown outcome s

(************* End baccarat engine (REPL) & helpers *************)

(************* Begin poker engine (REPL) & helpers *************)

(** [take_poker_command state] prompts the user to enter a poker command for 
    their turn in a betting round of Texas Hold'em. Returns a game_state with 
    the results of the betting round applied. *)
let rec take_poker_command state = 
  if remaining_players_n state <= 1 then state
  else if all_maxed_bets state then state
  else if all_bets_matched state && state.turn > (state.player_num - 1)
  then state
  else begin 
    let player_index = state.turn mod state.player_num in
    let p = List.nth state.players player_index in
    if p.in_game = false then
      take_poker_command {state with turn = state.turn + 1}
    else begin 
      start_turn state player_index;
      try match read_line() |> parse_p with
        | Check -> p_check_protocol p state
        | Raise -> p_raise_protocol p state
        | Call -> p_call_protocol p state
        | Fold -> p_fold_protocol p state
        | Remind -> p_remind_protocol p state
        | Quit -> quit_protocol state
        | Tools -> Tools.show_menu state.name p; take_poker_command state
      with 
      | Invalid_command -> invalid_protocol take_poker_command state
    end
  end

(** [previous_bet_player_h (player, state)] is the player who most recently
    betted before [player] in [state]. If [player] is the first to play,
    returns [player]. *)
and previous_bet_player_h (player, state) =
  if List.hd state.players = player && state.turn = 0 then player, state else
    let rec mod_pos a b =
      match a mod b with
      | x when x < 0 -> mod_pos (a + b) b
      | x -> x in
    let previous_player_index = mod_pos (find_index player state.players - 1)
        (state.player_num) in
    let previous_player = List.nth state.players previous_player_index in
    if previous_player.in_game
    then previous_player, state
    else previous_bet_player_h (previous_player, state)

(** [previous_bet_player player state] is the player who most recently
    betted before [player] in [state]. If no one has betted, it is
    the previous player. *)
and previous_bet_player player state =
  previous_bet_player_h (player, state) |> fst

(** [p_check_protocol player state] is [state] with [player] checking (matching
    the previous bet) if allowed and [state] with the turn restarted if checking
    is not allowed *)
and p_check_protocol player state =
  let check_allowed = all_bets_matched state in
  if check_allowed then take_poker_command {state with turn = state.turn + 1} 
  else begin
    print_endline invalid_check_msg;
    take_poker_command state
  end

(** [p_raise_protocol player state] is [state] with [player] raising
    (increasing the previous bet) if allowed and [state] with the turn restarted
    if raising is not allowed *)
and p_raise_protocol player state = 
  let previous_bet_player = previous_bet_player player state in
  let previous_bet = previous_bet_player.bet in
  if previous_bet >= player.money then begin
    print_string player.style bet_over_money_msg;
    take_poker_command state;
  end else
    let bet = Input.choose_num_geq_1_leq_n (bet_msg player.name)
        invalid_bet_msg 0 false in
    let can_bet = player.money - previous_bet in
    if bet > can_bet then begin
      bet_over_rem_msg (string_of_int can_bet) |> print_string player.style;
      p_raise_protocol player state
    end else begin
      player.bet <- previous_bet + bet;
      current_bet_msg player state.currency |> print_string player.style;
      take_poker_command {state with turn = state.turn + 1}
    end

(** [p_call_protocol player state] is [state] with [player] calling and
    advancing to the next turn if calling is allowed, and [state] prompting
    for new input if not allowed. *)
and p_call_protocol player state = 
  let previous_player = previous_bet_player player state in
  if previous_player.bet <= player.money then begin 
    player.bet <- previous_player.bet;
    current_bet_msg player state.currency |> print_string player.style;
    take_poker_command {state with turn = state.turn + 1}
  end else begin 
    print_endline "You do not have enough money to call";
    take_poker_command state
  end

(** [p_remind_protocol player state] prints the current bet of [player]
    and prompts for a new command. *)
and p_remind_protocol player state =
  current_bet_msg player state.currency |> print_string player.style;
  take_poker_command state

(** [p_fold_protocol] is [state] with [player] folding. *)
and p_fold_protocol player state =
  player.in_game <- false;
  let rem = player.money - player.bet in
  player.name ^ " folded and has " ^ string_of_int rem ^ " "
  ^ state.currency ^ " left.\n" |> print_string player.style;
  take_poker_command {state with turn = state.turn + 1}

(** [p_winners players s] is a list of winners in remaining [players] at the 
    end of a game of poker in [s].
    Requires: [players] is a non-empty list. *)
let p_winners players s =
  if List.length players = 1 then players
  else let hi_lo_sorted_p =
         List.map (fun p -> (p, Deck.concat p.hand s.flop.hand)) players
         |> List.sort (fun (_, h1) (_, h2) -> -(Poker.cmp_hand h1 h2)) in
    let rec top_n lst acc =
      match lst with
      | [] -> failwith "impossible, cannot have 0 players"
      | [(p, h)] -> p :: acc
      | (p1, h1) :: (p2, h2) :: t ->
        if Poker.cmp_hand h1 h2 = 0 then top_n ((p2, h2) :: t) (p1 :: acc)
        else p1 :: acc in
    top_n hi_lo_sorted_p []

(** [p_showdown s] pays the winning players in end of game [s] and charges
    the losing players. It also records statistics. *)
let p_showdown s =
  let remaining_players = List.filter (fun p -> p.in_game) s.players in
  let winners = p_winners remaining_players s in
  let total_bet = List.fold_left (fun acc p -> p.bet + acc) 0 s.players in
  let payment = total_bet / (List.length winners) in
  for i = 0 to s.player_num - 1 do
    let player = List.nth s.players i in
    let is_winner_i = List.mem player winners in
    if not is_winner_i then
      pay_player s.currency false "did not win, not printed" "lost"
        player.name player
    else begin player.money <- player.money - player.bet + payment;
      player.bet <- 0;
      player.name ^ " won and has " ^ string_of_int player.money
      ^ " " ^ s.currency ^ " total.\n" |> print_string player.style
    end;
    update_player_stats player.name
      (string_of_int player.money ^ " " ^ s.currency) s.name is_winner_i;
  done; reset_bets s; s

(** [reenter_all s] puts all the players back into the game after a round
    ends *)
let reenter_all s =
  List.iter (fun p -> p.in_game <- true) s.players; s

(** [poker_turn s] calls for betting rounds and reveals flop cards according to
    the rules of Texas Hold'em.  *)
let poker_turn s =
  let p_game s =
    (* 1st betting round *)
    print_endline "\nPre-flop round\n";
    let pre_flop_state = take_poker_command s in
    (* Check at least 2 players remaining *)
    if remaining_players_n pre_flop_state < 2 then pre_flop_state
    else begin 
      deal_n 3 pre_flop_state pre_flop_state.flop;
      (* 2nd betting round, reset turn to 0 first *)
      let reset_state = {pre_flop_state with turn = 0} in
      print_endline "\nFlop round\n";
      let flop_state = take_poker_command reset_state in
      (* Check at least 2 players remaining *)
      if remaining_players_n flop_state < 2 then flop_state
      else begin
        deal_n 1 flop_state flop_state.flop;
        (* 3rd betting round, reset turn to 0 first *)
        let reset_state = {flop_state with turn = 0} in
        print_endline "\nTurn round\n";
        let turn_state = take_poker_command reset_state in
        (* Check at least 2 players remaining *)
        if remaining_players_n turn_state < 2 then turn_state
        else begin
          deal_n 1 turn_state turn_state.flop;
          (* 4th and final betting round, reset turn to 0 first *)
          print_endline "\nRiver round\n";
          let reset_state = {turn_state with turn = 0} in
          take_poker_command reset_state
        end
      end
    end in
  p_game s |> p_showdown

(************* End poker engine (REPL) & helpers *************)

(************* Begin valid [game_info] values *************)

(** The abstract type representing meta game information for a given gamemode,
    which includes whether you are allowed to bet before cards are dealt, and
    if there exists a dealer, and the number of cards each player is dealt 
    initially. It also contains the appropriate function for executing game
    turns for the game mode. *)
type game_info = {
  init_bet : bool;
  has_dealer : bool;
  initial_cards : int;
  engine : t -> t;
}
let bj_info = {
  init_bet = Blackjack.init_bet;
  has_dealer = Blackjack.has_dealer;
  initial_cards = Blackjack.initial_cards;
  engine = bj_turn;
}
let poker_info = {
  init_bet = Poker.init_bet;
  has_dealer = Poker.has_dealer;
  initial_cards = Poker.initial_cards;
  engine = poker_turn
}
let ba_info = {
  init_bet = Baccarat.init_bet;
  has_dealer = Baccarat.has_dealer;
  initial_cards = Baccarat.initial_cards;
  engine = ba_turn
}

(************* End valid [game_info] values *************)

(************* Begin [game_info] getter functions *************)

(** [info str] returns the [game_info] associated with the game name, [str]. 
    Raises an error if [str] is not a supported game mode. *)
let info str = 
  if str = "blackjack" then bj_info
  else if str = "poker" then poker_info
  else if str = "baccarat" then ba_info
  else failwith "impossible, an invalid game name was used in shared_init ()" 

(** [get_meta state] prompts for gamemode name, and returns a tuple containing
    corresponding metadata, as well as [state] with [name] updated to the 
    gamemode entered by the user. *)
let get_meta state = 
  (* Select cardgame based on user input. *)
  let s = {state with name = Input.get_gamemode ()} in
  (* Display the rules of selected game. *)
  Tools.view_rules s.name Player.default_player false;
  print_endline "\n";

  (* Determine if players can make initial bets, and if there is a dealer
       for the selected game mode, and the number of cards each player begins 
       with. *)
  let game_info = info s.name in
  let init_bet = game_info.init_bet
  and has_dealer = game_info.has_dealer 
  and starting_cards = game_info.initial_cards 
  and engine = game_info.engine in
  init_bet, has_dealer, starting_cards, engine, s

(************* End [game_info] getter functions *************)

(************* Begin game initializer (REPL) functions *************)

(** [play_round init_bet has_dealer starting_cards state turn] enters a REPL 
    defined by [turn]. Players are prompted to place bets before cards are 
    dealt if [init_bet]. A dealer is assigned to the game if [has_dealer]. The
    number of cards dealt to each player before the game begins is 
    [starting cards]. *)
let rec play_round init_bet has_dealer starting_cards turn state =

  (* Check if there are still any eligible players remaining. *)
  if state.player_num <= 0 then begin 
    print_endline no_players_left_msg;
    quit_protocol state
  end else

    (* Resets game turn, all players and community decks, and resets the main
       deck to its initial size. If the game mode permits betting before
       cards are dealt, bets are also obtained from StdIn. *)
    let new_state = refresh_state state init_bet in
    (* Deal cards to every player, then the dealer if there is one. *)
    deal_all new_state starting_cards has_dealer;
    (* Update playcount of [state.name] *)
    Tools.update_total_game_stats state.name;

    (* Enter the REPL loop defined by [turn] to play the desired game, then 
        re-add all players (in case of folding). *)
    let final_state = turn new_state |> reenter_all in
    (* Checks if the game shall run another round. *)
    let replay_wanted = repeat_game_msg state.name |> Input.yes_or_no in
    if replay_wanted then begin

      (* Eliminate bankrupt players, begin next round. *)
      let next_state = eliminate_bankrupts final_state in
      play_round init_bet has_dealer starting_cards turn next_state
      (* Checks if a different game is desired instead. *)

    end else if Input.yes_or_no change_game_msg then begin

      (* Eliminate bankrupt players, prompt for new gamemode. *)
      let next_state = eliminate_bankrupts final_state in
      (* Prompts for new gamemode and updates metagame info. *)
      let init, has_deal, start_cards, engine', state' = get_meta next_state in
      (* Begins round of new gamemode. *)
      play_round init has_deal start_cards engine' state'

      (* Display final results & exit. *)
    end else display_final_scores final_state 

let game_constructor state =
  (* Prompt user for desired gamemode and obtain required meta info. *)
  let init, has_deal, start_cards, engine', state' = get_meta state in

  (* Creates a game state with number of decks, number of players, player names 
     and initial bets (if the game mode permits) *)
  let state'' = shared_init state' init_bet has_dealer start_cards in

  (* Begins the match loop *)
  play_round init has_deal start_cards engine' state''

(************* End game initializer (REPL) functions *************)