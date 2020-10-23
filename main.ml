open Deck
open Command
open Blackjack

(** By default, all players have this amount when generated, except the 
    dealer. *)
let default_allowance = 10

(** The abstract type representing a player. [name] is the name of the player,
    [hand] is the player's deck, [money] is the amount of money the player has 
    remaining. *)
type player = {
  name : string;
  mutable hand : Deck.t;
  mutable bet : int;
  mutable money : int;
}

let default_player = {
  name = "";
  hand = empty_deck ();
  bet = 0;
  money = default_allowance;
}

let dealer = {
  name = "dealer";
  hand = empty_deck ();
  bet = 0;
  money = max_int
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
}

let default_game = {
  name = "";
  game_deck = empty_deck ();
  player_num = 0;
  turn = 0;
  players = []
}

(** List of names of supported games. *)
let games = ["blackjack"]

(** Valid commands for each game. *)
let blackjack_rules = "Rules\n Hit: Take another card from the dealer.\n \
                       Stand: Take no more cards.\n Double down: Double your \
                       bet and commit to standing after one more hit.\n Quit: \
                       Quit the game.\n"
let poker_rules = "To be implemented"
let bridge_rules = "To be implemented"

let rules = [
  ("blackjack", blackjack_rules);
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
let invalid_command_msg = "You have entered an invalid command. Please try \
                           again.\n"
let bet_msg = "Please enter how much you wish to bet.\n"
let invalid_bet_msg = "You have entered an invalid bet. Please enter a number \
                       greater than 0.\n"
let goodbye_msg = "Goodbye!"

(** [print_enter_name n] is a string prompt for the user to enter the [n]-th 
    player's name. *)
let enter_name n = 
  Printf.sprintf "Please enter Player %d's name.\n" (n + 1)

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
  print_string input_prompt;
  let name = read_line () |> String.trim |> String.lowercase_ascii in 
  if List.exists (( = ) name) games then { default_game with name = name }
  else begin 
    print_endline no_such_game_msg; 
    choose_game () 
  end

(** [choose_num_geq_1 initial_prompt invalid_msg] prompts the player to enter 
    an int greater than 0 by printing [initial_prompt]. If the input is not 
    greater than 0, it prints [invalid_msg]. *)
let rec choose_num_geq_1 initial_prompt invalid_msg = 
  print_endline initial_prompt;
  print_string input_prompt;
  let n = read_line () |> int_of_string in
  if n > 0 then n else begin
    print_endline invalid_msg;
    choose_num_geq_1 initial_prompt invalid_msg
  end

(** [deck n state] is a game state equivalent to [state], except with the deck
    field updated to be a shuffled deck of [n] 52-card standard decks. *)
let deck state n : game_state =
  let deck = n |> n_std_decks |> shuffle in { state with game_deck = deck }

(** [player_helper n] takes a string from std input, and returns a new player 
    with an empty deck, no money, name equal to the entered string, and prompts
    the player to enter their bet. *)
let player_helper n = 
  n |> enter_name |> print_endline; 
  print_string input_prompt;
  let name = read_line () in
  let bet = choose_num_geq_1 bet_msg invalid_bet_msg in
  { default_player with name = name; bet = bet }

(** [create_player n acc index] is a player list with [n] players appended to 
    [acc]. The name of each player is taken from std input. 
    When calling [create player] for the first time, use [[]] for [acc] and [0]
    for index. *)
let rec create_players n acc index =
  match n with
  | 0 -> acc
  | n -> create_players (n - 1) (acc @ [player_helper index]) (index + 1)

(** [update_player n p s] is [s] with [player_num] equal to [n], and [players]
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
  player.hand |> string_of_deck |> ( ^ ) prefix |> print_endline

let start_turn state player = 
  player |> turn_msg state |> print_endline;
  print_hand state player;
  print_string input_prompt

(** [deal player state] deals a card from the game deck in [state] to the hand
    of [player]. *)
let rec deal player state = 
  match pick state.game_deck 0 with
  | None -> refill_deck state |> deal player
  | Some card -> begin
      player.hand <- append player.hand card;
      state.game_deck <- remainder state.game_deck
    end

(** Applies [deal player state] twice.
    TODO: make it deal_n *)
let deal_2 state player = 
  deal player state;
  deal player state

(** Deals 2 cards to all players in [state], then 2 cards to the dealer.
    TODO: extend to dealing n cards to all players in [state]. 
    find a way to make use of Blackjack.initial_cards in this function and
    its calls*)
let deal_all state = 
  let players = state.players in
  List.iter (deal_2 state) players;
  deal_2 state dealer

(** TODO: Implement a showdown function that :
    1) finds the dealer's score 
    2) creates a Blackjack.outcome value to feed into Blackjack.win_check
    3) iterate (or do it functionally) through every player and check if they
       beat the dealer using Deck.bj_score (If anything weird happens, remember
       that I decided to make a natural A/K, A/Q, A/J, A/10 blackjack have a 
       score of -1, I believe I've covered my bases but keep an eye out.)
    4) store the data any way you'd like, you need to use it in the next func *)
let showdown state = 
  failwith "unimplemented"  

(** TODO: Implement a function that pays out the players who win. We can 
    ignore losses for now because we are only playing a single round. 
    Print out the appropriate results (for each player, whether they win/lose,
    and their remaining money). Be sure to keep messages neat and easy to 
    maintain. Note how showdown and payout are used in [dealer_turn]. *)
let payout state data = 
  failwith "unimplemented"

(** Plays the dealer's turn, draws until deck score exceeds 17 *)
let dealer_turn state = 
  while bj_score dealer.hand < 17 do
    deal dealer state;
  done;
  showdown state |> payout state

(** Determines who's turn it is, then prompts and takes a command. Recurses
    to automatically begin the next action. *)
let rec game_turn s =
  let active_player_index = s.turn in
  if active_player_index >= s.player_num then dealer_turn s
  else
    let active_player = List.nth s.players active_player_index in
    start_turn s active_player_index;
    try match read_line() |> parse with
      | Hit -> hit_protocol active_player s 
      | Stand -> stand_protocol active_player s 
      | Double -> double_protocol active_player s 
      | Quit -> quit_protocol s
    with
    | Invalid_command -> invalid_protocol s

and invalid_protocol state = 
  print_endline invalid_command_msg;
  game_turn state 

and hit_protocol player state = 
  deal player state;
  game_turn state

and stand_protocol player state = 
  print_hand state state.turn;
  game_turn { state with turn = state.turn + 1 }

and double_protocol player state = 
  player.bet <- player.bet * 2;
  deal player state;
  stand_protocol player state

and quit_protocol state = 
  print_endline goodbye_msg;
  exit 0

let print_rules game = 
  List.assoc game rules |> print_endline 

let play_game () =

  (* Select cardgame *)
  let s = choose_game () in
  print_rules s.name;

  (* Select number of decks for game *)
  let n = choose_num_geq_1 number_of_decks_msg invalid_n_msg in

  (* Select number of players *)
  let num_players = choose_num_geq_1 number_of_players_msg invalid_p_msg in

  (* Name each player *)
  let players = create_players num_players [] 0 in

  (* Create initial gamestate with values entered above *)
  let s_with_decks = deck s n in
  let state = update_players num_players players s_with_decks in

  (* Deal cards to every player, then the dealer. *)
  deal_all state;

  (* Begins the game *)
  game_turn state

(** [main ()] welcomes the player and begins the game construction protocol. *)
let main () =
  print_string welcome_msg;
  play_game ()

(* Execute the game engine. *)
let () = main ()

