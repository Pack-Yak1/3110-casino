open Player
open Deck
open ANSITerminal
open Blackjack
open Command
open Tools

type player = Player.t

type t = {
  name : string;
  mutable game_deck : Deck.t;
  mutable flop : player;
  player_num : int;
  turn : int;
  mutable players : player list;
  currency : string;
  num_decks : int
}

let default_currency = "USD"
let currency_msg = "Please enter your unit of currency.\n"


let default_game = {
  name = "";
  game_deck = empty_deck ();
  flop = Player.flop;
  player_num = 0;
  turn = 0;
  players = [];
  currency = default_currency;
  num_decks = 0;
}

let input_prompt = "> "

let game_seln_msg = "Please enter the name of the game you want to play: \
                     Blackjack, Poker, Bridge.\n"
let no_such_game_msg = "You entered an invalid game name. Please try again.\n"
let number_of_decks_msg = "Please enter the number of decks to play with. It \
                           must be a number greater than 0.\n"
let no_entry_msg = "Please enter a number."
let invalid_p_msg = "You have entered an invalid number of players. Please \
                     enter a number greater than 0.\n"
let invalid_n_msg = "You have entered an invalid number of decks. Please enter \
                     a number greater than 0.\n"
let number_of_players_msg = "Please enter the number of players.\n"
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
                         may no longer check."
(** [turn_msg n] is a string prompt for the [n-th] user to enter their
    command. *)
let turn_msg (state : t) n = 
  let name = (List.nth state.players n).name in
  "It is now " ^ name ^ "'s turn.\n"
let yes_or_no_prompt = "\nDo you wish to play another round? (y/n)"
let yes_or_no_reminder = "Please enter either 'y' or 'n'.\n"
let final_score_header = "\nFinal Scores:\n"
let no_players_left_msg = "There are no players left in the game."
let elimination_msg name = 
  name ^ " is bankrupt and has been eliminated.\n"
let goodbye_msg = "Goodbye!"


(** List of names of supported games. *)
let games = ["blackjack"; "poker"]


let rec choose_game () =
  print_endline game_seln_msg;
  print_string [] input_prompt;
  let name = read_line () |> String.trim |> String.lowercase_ascii in 
  if List.exists (( = ) name) games
  then { default_game with name = name }
  else begin 
    print_endline no_such_game_msg; 
    choose_game () 
  end

(** [choose_num_geq_1_leq_n initial_prompt invalid_msg cap exists_limit]
    prompts the player to enter an int greater than 0 (and possibly less than
    or equal to [cap], depending on [exists_limit]) by printing
    [initial_prompt]. If the input is not greater than 0, or if the input is
    greater than [cap] and [exists_limit] is true, it prints [invalid_msg]. *)
let rec choose_num_geq_1_leq_n initial_prompt invalid_msg cap exists_limit = 
  print_endline initial_prompt;
  print_string [] input_prompt;
  let n = read_line () |> int_of_string_opt in
  match n with
  | Some num ->
    if exists_limit then
      if num > 0 && num <= cap then num else begin
        print_endline invalid_msg;
        choose_num_geq_1_leq_n initial_prompt invalid_msg cap exists_limit
      end else
    if num > 0 then num else begin
      print_endline invalid_msg;
      choose_num_geq_1_leq_n initial_prompt invalid_msg cap exists_limit
    end
  | None -> print_endline no_entry_msg;
    choose_num_geq_1_leq_n initial_prompt invalid_msg cap exists_limit



(** [update_players n p s] is [s] with [player_num] equal to [n], and [players]
    equal to [p]. *)
let update_players n p s =
  { s with player_num = n; players = p }

(** [deck state n] is a game state equivalent to [state], except with the deck
    field updated to be a shuffled deck of [n] 52-card standard decks. *)
let deck state n : t =
  let deck = n |> n_std_decks |> shuffle in { state with game_deck = deck }

(** [refill_deck state] is a shuffled standard 52-card deck. *)
let refill_deck state = 
  let top_up = std_deck () |> shuffle in
  { state with game_deck = top_up }

(** [print_hand state index] prints the hand of the player with 0-based index
    [index] in [state]. *)
let print_hand state index = 
  let player = List.nth state.players index in
  let prefix = "Your current hand is " in
  player.hand |> string_of_deck |> ( ^ ) prefix |> print_string player.style;
  print_string [] "\n"


let start_turn state player = 
  if length state.flop.hand > 0 then begin
    let flop_msg = "The community cards are: " ^ (state.flop.hand |> string_of_deck) in
    print_endline flop_msg;
  end else ();
  player |> turn_msg state |> print_endline;
  print_hand state player;
  print_string ((List.nth state.players player).style) input_prompt

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

(** [payout state win_msg loss_msg player_outcomes] pays out each player in
    [state] who wins with [win_msg] and charges each player who loses with
    [loss_msg], whose victory or lack thereof is given by
    the respective element of [player_outcomes]. It also prints the money for
    each player and updates statistics. *)
let payout state win_msg loss_msg player_outcomes =
  let players = state.players in
  for i = 0 to state.player_num - 1 do
    let player = List.nth players i in
    let win = List.nth player_outcomes i in
    pay_player state.currency win win_msg loss_msg player.name player;
    reset_bets state;
    update_player_stats player.name
      (string_of_int player.money ^ " " ^ state.currency) state.name win
  done; state

(** Plays the dealer's turn, draws until deck score exceeds 17 *)
let bj_dealer_turn state = 
  while bj_score dealer.hand < 17 do
    deal dealer state;
  done;
  print_string [] "Dealer's hand is ";
  dealer.hand |> Deck.string_of_deck |> print_endline;
  bj_showdown state |> payout state
    "won against the dealer" "lost against the dealer"

(** [bet_helper player] is [player] with prompted bet amount assigned. *)
let bet_helper (player : player) = 
  let msg = bet_msg player.name in
  let bet =
    choose_num_geq_1_leq_n msg invalid_bet_msg player.money true in
  { player with bet = bet }

(** [assign_bets_helper lst n acc] is a list of all [n] players in [lst] with 
    a bet of prompted value assigned to each. *)
let rec assign_bets_helper lst n acc =
  match lst with 
  | [] -> acc
  | h :: t -> 
    if n = 0 then acc else begin
      assign_bets_helper t (n - 1) (acc @ [bet_helper h])
    end

(** [assign_bets n state] is [state] with a bet (amount prompted) assigned
    to all [n] players in [state]. *)
let assign_bets n state = 
  assign_bets_helper state.players n []

(** [quit_protocol] ends the game instantly and prints a goodbye message.
    It is shared by all game modes. *)
let quit_protocol state = 
  print_endline goodbye_msg;
  exit 0

(** [invalid_protocol engine state] prompts for another input in the same
    state. *)
and invalid_protocol (engine : t -> t) (state : t) : t = 
  print_endline invalid_command_msg;
  engine state 

let rec bj_turn s : t =
  let active_player_index = s.turn in
  if active_player_index >= s.player_num then bj_dealer_turn s
  else
    let active_player = List.nth s.players active_player_index in
    start_turn s active_player_index;
    try match read_line() |> parse_bj with
      | Hit -> bj_hit_protocol active_player s 
      | Stand -> bj_stand_protocol active_player s 
      | Double -> bj_double_protocol active_player s
      | Quit -> quit_protocol s
      | Tools -> Tools.show_menu s.name active_player; bj_turn s
    with
    | Invalid_command -> invalid_protocol bj_turn s

and bj_hit_protocol player state = 
  deal player state;
  bj_turn state

and bj_stand_protocol player state = 
  print_hand state state.turn;
  bj_turn { state with turn = state.turn + 1 }

and bj_double_protocol player state =
  if player.bet <= player.money / 2 then begin
    player.bet <- player.bet * 2;
    deal player state;
    bj_stand_protocol player state
  end else begin
    print_endline invalid_double_msg;
    bj_turn state
  end

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
  | [] -> failwith "Not found"
  | h :: t -> if h = x then acc else find_index_h x (acc + 1) t

(** [find_index x lst] is the index of [x] in [lst] if it is present.
    If not, behavior is unspecified. *)
let find_index x lst =
  find_index_h x 0 lst

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
    let bet = choose_num_geq_1_leq_n (bet_msg player.name)
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
    end of a game of poker in [s]. *)
let p_winners players s =
  if List.length players = 1 then players
  else let hi_lo_sorted_p =
         List.map (fun p -> (p, Deck.concat p.hand s.flop.hand)) players
         |> List.sort (fun (_, h1) (_, h2) -> -(Poker.cmp_hand h1 h2)) in
    let rec top_n lst acc =
      match lst with
      | [] -> failwith "cannot have 0 players"
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
    end; update_player_stats player.name
      (string_of_int player.money ^ " " ^ s.currency) s.name is_winner_i;
  done; reset_bets s; s

(** [reenter_all s] puts all the players back into the game after a round
    ends *)
let reenter_all s =
  List.iter (fun p -> p.in_game <- true) s.players; s

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
      if remaining_players_n flop_state < 2 then flop_state
      else begin
        deal_n 1 flop_state flop_state.flop;
        (* 3rd betting round, reset turn to 0 first *)
        let reset_state = {flop_state with turn = 0} in
        print_endline "\nTurn round\n";
        let turn_state = take_poker_command reset_state in
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

(** [update_currency s] is [s] with unit of currency equal to the
    entered string. *)
let update_currency s =
  print_endline currency_msg;
  print_string [] input_prompt;
  let curr = read_line () in
  { s with currency = curr}


let shared_init s init_bet has_dealer starting_cards = 

  (* Select number of decks for game *)
  let n = choose_num_geq_1_leq_n number_of_decks_msg invalid_n_msg 0 false in

  (* Select number of players *)
  let num_players =
    choose_num_geq_1_leq_n number_of_players_msg invalid_p_msg 0 false in

  (* Name each player *)
  let players = Player.create_players num_players in

  (* Create initial gamestate with values entered above *)
  let s_with_decks = { (deck s n) with num_decks = n } in
  update_players num_players players s_with_decks |> update_currency

(** [wipe_hands state] is [state] with all players' hands set to empty. *)
let rec wipe_hands state = 
  let lst = state.players in
  match lst with
  | [] -> ()
  | h :: t -> h.hand <- empty_deck (); wipe_hands { state with players = t}

(** [refresh_state state init_bet] is [state] with all players' and dealer's
    hands set to empty, decks shuffled, flop empty, overall stats updated.
    Initial bets are assigned if [init_bet]. *)
let refresh_state state init_bet = 
  update_total_game_stats state.name;
  dealer.hand <- empty_deck ();
  wipe_hands state;
  state.game_deck <- n_std_decks state.num_decks |> shuffle;
  state.flop.hand <- empty_deck ();
  List.iter (fun p -> p.in_game <- true) state.players;
  if init_bet then begin 
    let new_players = assign_bets state.player_num state in
    { state with turn = 0; players = new_players }
  end else { state with turn = 0 }

(** [display_final_scores state] prints the final scores of all players
    in [state]. *)
let display_final_scores state = 
  print_endline final_score_header;
  let lst = state.players in
  List.iter (Player.print_score state.currency) lst

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

(** Prompts for a yes or no response to whether another round is desired. *)
let rec replay () = 
  print_endline yes_or_no_prompt;
  print_string [] input_prompt;
  let response = read_line () |> String.trim |> String.lowercase_ascii in
  if response = "y" then true else if response = "n" then false
  else begin 
    print_endline yes_or_no_reminder;
    replay ()
  end

let rec play_round init_bet has_dealer starting_cards state turn =
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

    let final_state = turn new_state |> reenter_all in

    (* Checks if the game shall run another round *)
    let replay_wanted = replay () in
    if replay_wanted then begin
      let next_state = eliminate_bankrupts final_state in
      play_round init_bet has_dealer starting_cards next_state turn 
    end else display_final_scores final_state