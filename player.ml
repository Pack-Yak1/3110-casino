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

(** [add_new_player_stats_h lst name] is [lst] with
    a new player with name [name] added. If there already exists a player
    with name [name], it is [lst]. *)
let add_new_player_stats_h lst name =
  let match_name name p =
    match p with
    | `Assoc ["Name", `String n; m; g1; g2; g3] ->
      if n = name then true else false
    | _ -> failwith "wrong json format" in
  if List.exists (match_name name) lst then lst else
    `Assoc ["Name", `String name; "Money", `String "";
            "Blackjack", `Assoc ["Plays", `Int 0; "Wins", `Int 0];
            "Poker", `Assoc ["Plays", `Int 0; "Wins", `Int 0];
            "Baccarat", `Assoc ["Plays", `Int 0; "Wins", `Int 0]] :: lst

(** [add_new_player_stats name] adds a new player with name [name] with
    no money data, plays, and wins to statistics if a player does not already
    exist in stats with that name. If player already exists, does nothing. *)
let add_new_player_stats name =
  let j = Yojson.Basic.from_file "stats.json" in
  let assoc = j |> member "Players" in
  let total = j |> member "Total games played" in
  match assoc with
  | `List lst ->
    let new_players = add_new_player_stats_h lst name in
    `Assoc ["Players", `List new_players; "Total games played", total]
    |> Yojson.Basic.to_file "stats.json"
  | _ -> failwith "wrong json format"


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
    begin (add_new_player_stats name);
      { default_player with name = name }, (name :: names)
    end

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
