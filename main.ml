open Deck
open Command
open Blackjack
open Poker
open ANSITerminal
open Tools

(** By default, all players have this amount when generated, except the 
    dealer. *)
let default_allowance = 100

(** By default, the in-game currency is USD. *)
let default_currency = "USD"

(** The abstract type representing a player. [name] is the name of the player,
    [hand] is the player's deck, [money] is the amount of money the player has 
    remaining. *)
type player = {
  name : string;
  mutable hand : Deck.t;
  mutable bet : int;
  mutable money : int;
  mutable style : ANSITerminal.style list
}

let default_player = {
  name = "";
  hand = empty_deck ();
  bet = 0;
  money = default_allowance;
  style = [Foreground Red]
}

let dealer = {
  name = "dealer";
  hand = empty_deck ();
  bet = 0;
  money = max_int;
  style = []
}

(** The abstract type representing a turn in a game. [name] is the name of the
    cardgame being played. [game_deck] is the main deck on the table from which
    players must draw. [turn] is the 0-index of the current player in 
    [players] to play. [player_num] is the number of players in the game. *)
type game_state = {
  name : string;
  mutable game_deck : Deck.t;
  player_num : int;
  turn : int;
  mutable players : player list;
  currency : string;
  num_decks : int
}

let default_game = {
  name = "";
  game_deck = empty_deck ();
  player_num = 0;
  turn = 0;
  players = [];
  currency = default_currency;
  num_decks = 0;
}

(** List of names of supported games. *)
let games = ["blackjack"; "poker"]

(** Valid commands for each game. *)
let blackjack_rules = "Rules\n Hit: Take another card from the dealer.\n \
                       Stand: Take no more cards.\n Double down: Double your \
                       bet and commit to standing after one more hit.\n Quit: \
                       Quit the game.\n Tools: View or edit settings and \
                       rules."
(** TODO: Finish typing rules *)
let poker_rules = "Rules\n Check: Decline to bet. You keep your hand but do \
                   not open.\n 
                   Stand: Take no more cards.\n Double down: Double your \
                   bet and commit to standing after one more hit.\n Quit: \
                   Quit the game.\n Tools: View or edit settings and \
                   rules."
let bridge_rules = "To be implemented"

let rules = [
  ("blackjack", blackjack_rules);
  ("poker", poker_rules)
]

(* Begin ingame prompts: *)
let input_prompt = "> "
let welcome_msg = "\n\nWelcome to the 3110 Casino.\n"
let game_seln_msg = "Please enter the name of the game you want to play.\n"
let no_such_game_msg = "You entered an invalid game name. Please try again.\n"

let number_of_decks_msg = "Please enter the number of decks to play with. It \
                           must be a number greater than 0.\n"
let invalid_n_msg = "You have entered an invalid number of decks. Please enter \
                     a number greater than 0.\n"

let number_of_players_msg = "Please enter the number of players.\n"
let invalid_p_msg = "You have entered an invalid number of players. Please \
                     enter a number greater than 0.\n"

let invalid_bet_msg = "You have entered an invalid bet. Please enter a number \
                       greater than 0 and less than your limit.\n"
let bet_msg player_name =
  player_name ^  ", please enter how much you wish to bet.\n"

let currency_msg = "Please enter your unit of currency.\n"

let invalid_command_msg = "You have entered an invalid command. Please try \
                           again.\n"
let invalid_double_msg = "You do not have enough money to double."

let no_entry_msg = "Please enter a number."

let yes_or_no_prompt = "Do you wish to play another round? (y/n)"
let response_yes = "y"
let response_no = "n"
let yes_or_no_reminder = "Please enter either 'y' or 'n'.\n"

let no_players_left_msg = "There are no players left in the game."
let elimination_msg name = 
  name ^ " is bankrupt and has been eliminated.\n"

let print_score (player : player) = 
  let msg = player.name ^ " has $" ^ (string_of_int player.money) ^ "." in
  print_endline msg
let final_score_header = "\nFinal Scores:\n"

let goodbye_msg = "Goodbye!"

(** [enter_name n] is a string prompt for the user to enter the [n]-th 
    player's name. *)
let enter_name n = 
  Printf.sprintf "Please enter Player %d's name.\n" (n + 1)

(** [update_currency s] is [s] with unit of currency equal to the
    entered string. *)
let update_currency s =
  print_endline currency_msg;
  print_string [] input_prompt;
  let curr = read_line () in
  { s with currency = curr}

(** [turn_msg n] is a string prompt for the [n-th] user to enter their 
    command. *)
let turn_msg (state : game_state) n = 
  let name = (List.nth state.players n).name in
  "It is now " ^ name ^ "'s turn.\n"
(* End ingame prompts *)

(** [choose_game name] is a game state with an empty deck, 1 player and game 
    name equal to [name] if [name] is a supported game. If not, the player is
    prompted to enter a valid game name again. *)
let rec choose_game () =
  print_endline game_seln_msg;
  print_string [] input_prompt;
  let name = read_line () |> String.trim |> String.lowercase_ascii in 
  if List.exists (( = ) name) games then { default_game with name = name }
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

(** [deck n state] is a game state equivalent to [state], except with the deck
    field updated to be a shuffled deck of [n] 52-card standard decks. *)
let deck state n : game_state =
  let deck = n |> n_std_decks |> shuffle in { state with game_deck = deck }

(** [player_helper n] takes a string from std input, and returns a 
    new player with an empty deck, default money, name equal to the entered 
    string. *)
let player_helper n = 
  n |> enter_name |> print_endline; 
  print_string [] input_prompt;
  let name = read_line () in
  { default_player with name = name }

let rec create_players_helper n acc index =
  match n with
  | 0 -> acc
  | n -> begin
      let lst = acc @ [player_helper index] in
      create_players_helper (n - 1) (lst) (index + 1)
    end

(** [create_players n init_bet] is a player list with [n] players appended to 
    [acc]. The name of each player is taken from std input. *)
let create_players n = 
  create_players_helper n [] 0

(** [update_players n p s] is [s] with [player_num] equal to [n], and [players]
    equal to [p]. *)
let update_players n p s =
  { s with player_num = n; players = p }

(** [refill_deck state] is a shuffled standard 52-card deck. *)
let refill_deck state = 
  let top_up = std_deck () |> shuffle in
  { state with game_deck = top_up }

let print_hand state index = 
  let player = List.nth state.players index in
  let prefix = "Your current hand is " in
  player.hand |> string_of_deck |> ( ^ ) prefix |> print_string player.style;
  print_string [] "\n"

let start_turn state player = 
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

(** Deals [n] cards to all players in [state], then [n] cards to the dealer. *)
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

(** [pay_player win n p] pays a player with index [n] if they win, given by
    [win], or charges them if they do not win, and prints the result of
    whether they win/lose and their remaining money. *)
let pay_player state win n p =
  let player_n = "Player " ^ string_of_int (n + 1) in
  if win then begin
    p.money <- p.bet + p.money;
    player_n ^ " won against the dealer and has " ^ string_of_int p.money
    ^ " " ^ state.currency ^ " total." |> print_endline;
  end else begin
    p.money <- p.money - p.bet;
    player_n ^ " lost against the dealer and has "  ^ string_of_int p.money
    ^ " " ^ state.currency ^ " total."|> print_endline;
  end

(** [payout state player_outcomes] pays out each player in [state] who wins
    and charges each player who loses, whose victory or lack thereof is given by
    the respective element of [player_outcomes]. It also prints the win/loss
    and money for each player. *)
let payout state player_outcomes =
  let players = state.players in
  for i = 0 to state.player_num - 1 do
    pay_player state (List.nth player_outcomes i) i (List.nth players i)
  done; state

(** Plays the dealer's turn, draws until deck score exceeds 17 *)
let dealer_turn state = 
  while bj_score dealer.hand < 17 do
    deal dealer state;
  done;
  bj_showdown state |> payout state

(** Prints the rules of the game with name [name] *)
let print_rules name = 
  List.assoc name rules |> print_endline 

let bet_helper (player : player) = 
  let msg = bet_msg player.name in
  let bet =
    choose_num_geq_1_leq_n msg invalid_bet_msg player.money true in
  { player with bet = bet }

let rec assign_bets_helper lst n acc =
  match lst with 
  | [] -> acc
  | h :: t -> 
    if n = 0 then acc else begin
      assign_bets_helper t (n - 1) (acc @ [bet_helper h])
    end

let assign_bets n state = 
  assign_bets_helper state.players n []

(** [quit_protocol] ends the game instantly and prints a goodbye message.
    It is shared by all game modes. *)
let quit_protocol state = 
  print_endline goodbye_msg;
  exit 0

(** Determines whose turn it is, then prompts and takes a command. Recurses
    to automatically begin the next action. *)
let rec bj_turn s : game_state =
  let active_player_index = s.turn in
  if active_player_index >= s.player_num then dealer_turn s
  else
    let active_player = List.nth s.players active_player_index in
    start_turn s active_player_index;
    try match read_line() |> parse_bj with
      | Hit -> bj_hit_protocol active_player s 
      | Stand -> bj_stand_protocol active_player s 
      | Double -> bj_double_protocol active_player s
      | Quit -> quit_protocol s
      | Tools -> let _ = print_rules s.name in bj_turn s
    with
    | Invalid_command -> bj_invalid_protocol s

and bj_invalid_protocol state = 
  print_endline invalid_command_msg;
  bj_turn state 

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

(** TODO: Implement a function that runs a single game of Texas holdem *)
let poker_turn s = 
  failwith "unimplemented"

(** The abstract type representing meta game information for a given gamemode,
    which includes whether you are allowed to bet before cards are dealt, and
    if there exists a dealer, and the number of cards each player is dealt 
    initially. It also contains the appropriate function for executing game
    turns for the game mode. *)
type game_info = {
  init_bet : bool;
  has_dealer : bool;
  initial_cards : int;
  turn : game_state -> game_state;
}

let bj_info = {
  init_bet = true;
  has_dealer = true;
  initial_cards = Blackjack.initial_cards;
  turn = bj_turn;
}

let poker_info = {
  init_bet = false;
  has_dealer = false;
  initial_cards = Poker.initial_cards;
  turn = poker_turn
}

(** [info str] returns the [game_info] associated with the game name, [str]. 
    Raises an error if [str] is not a supported game mode. *)
let info str = 
  if str = "blackjack" then bj_info
  else if str = "poker" then poker_info
  else failwith "An invalid game name was used in shared_init ()" 

(** [shared_init s init_bet has_dealer starting_cards turn] is a game state 
    with number of decks, number of players, player names and initial bets 
    (iff [init_bet] is true) taken from user responses in StdIn. The state 
    initializes a dealer iff [has_dealer] is true, and deals each player 
    [starting_cards]. The game state returned is [s] mutated. *)
let shared_init s init_bet has_dealer starting_cards = 

  (* Select number of decks for game *)
  let n = choose_num_geq_1_leq_n number_of_decks_msg invalid_n_msg 0 false in

  (* Select number of players *)
  let num_players =
    choose_num_geq_1_leq_n number_of_players_msg invalid_p_msg 0 false in

  (* Name each player *)
  let players = create_players num_players in

  (* Create initial gamestate with values entered above *)
  let s_with_decks = { (deck s n) with num_decks = n } in
  update_players num_players players s_with_decks |> update_currency

let rec wipe_hands state = 
  let lst = state.players in
  match lst with
  | [] -> ()
  | h :: t -> h.hand <- empty_deck (); wipe_hands { state with players = t}

let refresh_state state init_bet = 
  dealer.hand <- empty_deck ();
  wipe_hands state;
  state.game_deck <- n_std_decks state.num_decks |> shuffle;
  if init_bet then begin 
    let new_players = assign_bets state.player_num state in
    { state with turn = 0; players = new_players }
  end else { state with turn = 0 }

let display_final_scores state = 
  print_endline final_score_header;
  let lst = state.players in
  List.iter (print_score) lst

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

let rec play_round init_bet has_dealer starting_cards state turn =
  (* Check if there are still any eligible players remaining. *)
  if state.player_num <= 0 then begin 
    print_endline no_players_left_msg;
    quit_protocol state
  end else
    (* Resets game turn, all players and community decks (TODO: community deck 
       not implemented yet), and resets the main deck to its initial size. If 
       the game mode permits betting before cards are dealt, bets are also 
       obtained from StdIn. *)
    let new_state = refresh_state state init_bet in

    (* Deal cards to every player, then the dealer if there is one. *)
    deal_all new_state starting_cards has_dealer;

    (* Begins the match *)
    let final_state = turn new_state in
    (* Match has ended *)

    (* Checks if the game shall run another round *)
    let replay_wanted = replay () in
    if replay_wanted then begin
      let next_state = eliminate_bankrupts final_state in
      play_round init_bet has_dealer starting_cards next_state turn 
    end else display_final_scores final_state

(** Prompts for a yes or no response to whether another round is desired. *)
and replay () = 
  print_endline yes_or_no_prompt;
  print_string [] input_prompt;
  let response = read_line () |> String.trim |> String.lowercase_ascii in
  if response = response_yes then true 
  else if response = response_no then false
  else begin 
    print_endline yes_or_no_reminder;
    replay ()
  end

(** [main ()] welcomes the player and begins the game construction protocol. *)
let main () =
  print_string [] welcome_msg;

  (* Select cardgame *)
  let s = choose_game () in
  print_rules s.name;

  (* Determine if players can make initial bets, and if there is a dealer
       for the selected game mode, and the number of cards each player begins 
       with. *)
  let game_info = info s.name in
  let init_bet = game_info.init_bet
  and has_dealer = game_info.has_dealer 
  and starting_cards = game_info.initial_cards 
  and turn = game_info.turn in

  (* Creates a game state with number of decks, number of players, player names 
     and initial bets (if the game mode permits) *)
  let state = shared_init s init_bet has_dealer starting_cards in

  (* Begins the match loop *)
  play_round init_bet has_dealer starting_cards state turn 

(* Execute the game engine. *)
let () = main ()
