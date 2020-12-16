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

(** [main ()] welcomes the player and initializes the game constructor*)
let main () = 
  print_string [] welcome_msg;
  Gamestate.default_game |> Gamestate.game_constructor

(* Execute the game engine. *)
let () = main ()
