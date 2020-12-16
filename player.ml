open Deck
open Yojson.Basic.Util

type t = {
  name : string;
  mutable hand : Deck.t;
  mutable bet : int;
  mutable money : int;
  mutable in_game : bool;
  mutable bet_on : Baccarat.outcome;
  mutable style : ANSITerminal.style list;
}

let default_allowance = 100

let default_player = {
  name = "";
  hand = empty_deck ();
  bet = 0;
  money = default_allowance;
  in_game = true;
  bet_on = Tie;
  style = []
}

let dealer = {
  name = "dealer";
  hand = empty_deck ();
  bet = 0;
  money = max_int;
  in_game = true;
  bet_on = Tie;
  style = []
}

let flop = {
  name = "flop";
  hand = empty_deck ();
  bet = 0;
  money = 0;
  in_game = true;
  bet_on = Tie;
  style = [];
}

let input_prompt = "> "

(** [enter_name n] is a string prompt for the user to enter the [n]-th 
    player's name. *)
let enter_name n = 
  Printf.sprintf "Please enter Player %d's name.\n" (n + 1)

let ends_x x s =
  let sub_safe super start len sub =
    try let substring = String.sub super start len in substring = sub with
    | Invalid_argument _ -> false in
  let len_s = String.length s in
  let len_x = String.length x in
  let start = len_s - len_x in
  sub_safe s start len_x x

(** [player_helper n names] takes a string from std input, and returns a 
    new player with an empty deck, default money, name equal to the entered 
    string iff there does not already exist a player with the name in [names].
    If there already exists such a player, prompts for a new name. *)
let rec player_helper n names = 
  n |> enter_name |> print_endline; 
  print_string input_prompt;
  let name = read_line () in
  if List.mem name names then begin
    print_endline "Cannot use the same name as another player.\n";
    player_helper n names end else
  if ends_x "(copy)" name then begin
    print_endline {|Cannot use "(copy)" at the end of a name.|};
    player_helper n names end else
    { default_player with name = name }, (name :: names)

let rec create_players_helper n acc index names =
  match n with
  | 0 -> acc
  | n -> begin
      let new_player, names = player_helper index names in
      let lst = acc @ [new_player] in
      create_players_helper (n - 1) (lst) (index + 1) names
    end

let create_players n = 
  create_players_helper n [] 0 []


let print_score (curr : string) (player : t) = 
  let msg = player.name ^ " has " ^ (string_of_int player.money) ^ " "
            ^ curr ^ "." in
  print_endline msg
