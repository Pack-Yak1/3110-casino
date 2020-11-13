open Deck

type t = {
  name : string;
  mutable hand : Deck.t;
  mutable bet : int;
  mutable money : int;
  mutable in_game : bool;
  mutable style : ANSITerminal.style list;
}

let default_allowance = 100

let default_player = {
  name = "";
  hand = empty_deck ();
  bet = 0;
  money = default_allowance;
  in_game = true;
  style = []
}

let dealer = {
  name = "dealer";
  hand = empty_deck ();
  bet = 0;
  money = max_int;
  in_game = true;
  style = []
}

let input_prompt = "> "

(** [enter_name n] is a string prompt for the user to enter the [n]-th 
    player's name. *)
let enter_name n = 
  Printf.sprintf "Please enter Player %d's name.\n" (n + 1)

(** [player_helper n] takes a string from std input, and returns a 
    new player with an empty deck, default money, name equal to the entered 
    string. *)
let player_helper n = 
  n |> enter_name |> print_endline; 
  print_string input_prompt;
  let name = read_line () in
  { default_player with name = name }

let rec create_players_helper n acc index =
  match n with
  | 0 -> acc
  | n -> begin
      let lst = acc @ [player_helper index] in
      create_players_helper (n - 1) (lst) (index + 1)
    end

let create_players n = 
  create_players_helper n [] 0


let print_score (curr : string) (player : t) = 
  let msg = player.name ^ " has $" ^ (string_of_int player.money) ^ " "
            ^ curr ^ "." in
  print_endline msg
