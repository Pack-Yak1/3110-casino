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
  engine : game_state -> game_state;
}

let bj_info = {
  init_bet = true;
  has_dealer = true;
  initial_cards = Blackjack.initial_cards;
  engine = Gamestate.bj_turn;
}

let poker_info = {
  init_bet = false;
  has_dealer = false;
  initial_cards = Poker.initial_cards;
  engine = poker_turn
}

(** [info str] returns the [game_info] associated with the game name, [str]. 
    Raises an error if [str] is not a supported game mode. *)
let info str = 
  if str = "blackjack" then bj_info
  else if str = "poker" then poker_info
  else failwith "An invalid game name was used in shared_init ()" 


(** [main ()] welcomes the player and begins the game construction protocol. *)
let main () =
  print_string [] welcome_msg;

  (* Select cardgame *)
  let s = Gamestate.choose_game () in
  Tools.view_rules Player.default_player s.name;
  print_endline "\n";

  (* Determine if players can make initial bets, and if there is a dealer
       for the selected game mode, and the number of cards each player begins 
       with. *)
  let game_info = info s.name in
  let init_bet = game_info.init_bet
  and has_dealer = game_info.has_dealer 
  and starting_cards = game_info.initial_cards 
  and turn = game_info.engine in

  (* Creates a game state with number of decks, number of players, player names 
     and initial bets (if the game mode permits) *)
  let state = Gamestate.shared_init s init_bet has_dealer starting_cards in

  (* Begins the match loop *)
  Gamestate.play_round init_bet has_dealer starting_cards state turn

(* Execute the game engine. *)
let () = main ()
