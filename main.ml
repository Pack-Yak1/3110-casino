open Deck
open Command

exception Invalid_number 

(* We may need a separate module for player or game_state, but I
   am just leave it here right now for convenience.*)
type player = {
  name : string;
  hand : card list;
  money : float;
}

type game_state = {
  name : string;
  game_deck : card list;
  player_num : int;
  players : player list;
}

(* Games we support, not completed. *)
let games = ["blackjack"]

let rec initialize = function
  | 0 -> []
  | n -> std_deck() @ initialize (n - 1)

let shuffle cards = 
  let random = List.map (fun n -> (Random.bits(), n)) cards in 
  let sorted = List.sort compare random in 
  List.map (fun (a, b) -> b) sorted

let rec return n cards = match n with
  | 0 -> []
  | n -> List.hd cards :: return (n - 1) (List.tl cards)

let rec choose_game name =
  let name = name |> String.trim |> String.lowercase_ascii in 
  if List.exists (fun a -> a = name) games then {
    name = name;
    deck = [];
    player_num = 1;
    players = [];
  }
  else choose_game (read_line())

let deck n s =
  let game_deck = n |> initialize |> shuffle in
  {
    name = s.name;
    deck = game_deck;
    player_num = 1;
    players = [];
  }

let player_helper str = { 
  name = str;
  hand = [];
  money = 0.0;
}

let rec init_player = function 
  | 0 -> []
  | n -> player_helper " " :: init_player (n - 1)

let update_name x =
  print_endline "Please enter your name.\n";
  let n = read_line() in 
  player_helper n

let create_player n =
  n |> init_player |> List.map update_name

let update_player n p s =
  {name = s.name;
   deck = s.deck;
   player_num = n;
   players = p;
  }

let cycle_helper =
  print_endline "Please enter your command.\n";
  parse (read_line())

let cycle s =
  let players = s.players in
  List.iter cycle_helper players

(* Not done yet *)
let play_game name =
  let s = choose_game name in
  print_endline "Please enter the number of decks you want.\n";
  let nd = int_of_string (read_line()) in 
  let s' = deck nd s in
  print_endline "Please enter the number of players.\n";
  let np = int_of_string (read_line()) in
  let players = create_player np in
  let game = update_player np players s' in
  cycle game

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_string "\n\nWelcome to the 3110 Text Adventure Game engine.\n";
  print_endline "Please enter the name of the game you want to play.\n";
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | name -> play_game name

(* Execute the game engine. *)
let () = main ()

