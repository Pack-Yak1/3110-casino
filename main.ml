open Deck
open Command
open Blackjack
open Poker
open ANSITerminal
open Tools
open Player
open Gamestate
open Poker

type player = Player.t

type game_state = Gamestate.t

(* Begin ingame prompts: *)
let input_prompt = "> "
let welcome_msg = "\n\nWelcome to the 3110 Casino.\n"
(* End ingame prompts *)

(** The abstract type representing meta game information for a given gamemode,
    which includes whether you are allowed to bet before cards are dealt, and
    if there exists a dealer, and the number of cards each player is dealt 
    initially. It also contains the appropriate function for executing game
    turns for the game mode. *)
type game_info = {
  init_bet : bool;
  has_dealer : bool;
  initial_cards : int;
  engine : game_state -> game_state;
}

let bj_info = {
  init_bet = Blackjack.init_bet;
  has_dealer = Blackjack.has_dealer;
  initial_cards = Blackjack.initial_cards;
  engine = Gamestate.bj_turn;
}

let poker_info = {
  init_bet = Poker.init_bet;
  has_dealer = Poker.has_dealer;
  initial_cards = Poker.initial_cards;
  engine = Gamestate.poker_turn
}

let ba_info = {
  init_bet = Baccarat.init_bet;
  has_dealer = Baccarat.has_dealer;
  initial_cards = Baccarat.initial_cards;
  engine = Gamestate.ba_turn
}

(** [info str] returns the [game_info] associated with the game name, [str]. 
    Raises an error if [str] is not a supported game mode. *)
let info str = 
  if str = "blackjack" then bj_info
  else if str = "poker" then poker_info
  else if str = "baccarat" then ba_info
  else failwith "An invalid game name was used in shared_init ()" 


(** [main ()] welcomes the player and begins the game construction protocol. *)
let main () =
  print_string [] welcome_msg;

  (* Select cardgame *)
  let s = Gamestate.choose_game () in
  Tools.view_rules s.name Player.default_player false;
  print_endline "\n";
  if s.name = "poker"
  then "The blinds are " ^ string_of_int s.small_blind ^ "/"
       ^ string_of_int (s.small_blind * 2) ^ ".\n" |> print_endline
  else ();

  (* Determine if players can make initial bets, and if there is a dealer
       for the selected game mode, and the number of cards each player begins 
       with. *)
  let game_info = info s.name in
  let init_bet = game_info.init_bet
  and has_dealer = game_info.has_dealer 
  and starting_cards = game_info.initial_cards 
  and engine = game_info.engine in

  (* Creates a game state with number of decks, number of players, player names 
     and initial bets (if the game mode permits) *)
  let state = Gamestate.shared_init s init_bet has_dealer starting_cards in

  (* Begins the match loop *)
  Gamestate.play_round init_bet has_dealer starting_cards state engine

(* Execute the game engine. *)
let () = main ()
