open Player
open ANSITerminal
open Yojson.Basic.Util

type player = Player.t

type color = ANSITerminal.color

type tools_command =
  | Set_Color
  | View_Rules
  | View_Statistics
  | Return
  | Invalid

(** Printed before user input *)
let input_prompt = "> "

(** Valid commands for each game. *)
let blackjack_rules = "Rules\nHit: Take another card from the dealer.\n\
                       Stand: Take no more cards.\nDouble down: Double your \
                       bet and commit to standing after one more hit.\nSplit: \
                       Split your hand into two and take one more card for \
                       each new hand, placing a bet equal to your first on \
                       the second hand. All 10-value cards are treated the \
                       same. Further splitting of post-split hands is not \
                       allowed. Doubling post-split hands is not allowed. \
                       An ace and ten value card after a split are counted \
                       as a blackjack.\nQuit: \
                       Quit the game.\nTools: View or edit settings and \
                       rules."
let poker_rules = "Rules\nCheck: Decline to bet. You keep your hand but do \
                   not open.\n\
                   Call: Match the bet of the previous player.\n\
                   Raise: Bet more than the previous bet. Enter the amount \
                   by which you want to increase your total bet, across all \
                   rounds.\n\
                   Fold: Abandon your bet and your cards, leaving the round.\n\
                   Quit the game.\nTools: View or edit settings and \
                   rules."
let baccarat_rules = "Rules\n There are two hands at the deck, Banker and Player.\
                      You can bet on Banker, Player, or Tie before drawing cards.\
                      Whoever closer to 9 wins."

let rules = [
  ("blackjack", blackjack_rules);
  ("poker", poker_rules);
  ("baccarat", baccarat_rules)
]

(** [str_of_game_player game p] represents the play data for the game with
    name [game] and for [p] *)
let str_of_game_player game p =
  let game_data = p |> member game in
  let plays = game_data |> member "Plays" |> to_int in
  let wins = game_data |> member "Wins" |> to_int in
  "\n" ^ game ^ ": " ^
  "\n  Plays: " ^ string_of_int plays ^
  "\n  Wins: " ^ string_of_int wins

(** [str_of_all_games_player p] represents the play data for all games
    for [p] *)
let str_of_all_games_player p =
  let bj_str = str_of_game_player "Blackjack" p in
  let poker_str = str_of_game_player "Poker" p in
  let baccarat_str = str_of_game_player "Baccarat" p in
  bj_str ^ poker_str ^ baccarat_str

(** [print_stats_player j_player player] prints the statistics for
    [j_player] during [player]'s turn *)
let print_stats_player j_player player =
  let name = j_player |> member "Name" |> to_string in
  let money = j_player |> member "Money" |> to_string in
  let all_games = str_of_all_games_player j_player in
  "Name: " ^ name ^
  "\nMoney: " ^ money ^
  all_games ^ "\n\n" |> print_string player.style

(** [str_of_total_stats j] represents the play data in [j] for all games *)
let str_of_total_stats j =
  let total_data = j |> member "Total games played" in
  let bj_plays = total_data |> member "Blackjack" |> to_int |> string_of_int in
  let poker_plays = total_data |> member "Poker" |> to_int |> string_of_int in
  let bct_plays = total_data |> member "Baccarat" |> to_int |> string_of_int in
  "Total games played: " ^
  "\n  Blackjack: " ^ bj_plays ^
  "\n  Poker: " ^ poker_plays ^
  "\n  Baccarat: " ^ bct_plays

(** [update_game win g] updates the number of wins and plays for game [g].
    The number of wins is incremented iff [win] is true. *)
let update_game win g =
  match g with
  | `Assoc ["Plays", `Int plays; "Wins", `Int wins] ->
    if win then `Assoc ["Plays", `Int (plays + 1); "Wins", `Int (wins + 1)]
    else `Assoc ["Plays", `Int (plays + 1); "Wins", `Int wins]
  | _ -> failwith "not a game"

(** [update_if_match name money game win player] is [player] with money
    set to to [money] and the number of wins and plays accurately incremented,
    in the game with name [game], iff [player] has name [name]. Iff [player]
    has name [name] and [win] is true, the number of wins is incremented.
    Otherwise, return [player]. *)
let update_if_match name money game win player =
  let game = String.capitalize_ascii game in
  match player with
  | `Assoc ["Name", n; "Money", m; g1; g2; g3] ->
    if n |> to_string = name then
      if fst g1 = game
      then `Assoc ["Name", n; "Money", `String money;
                   game, member game player |> update_game win; g2; g3] else
      if fst g2 = game
      then `Assoc ["Name", n; "Money", `String money;
                   g1; game, member game player |> update_game win; g3] else
      if fst g3 = game
      then `Assoc ["Name", n; "Money", `String money;
                   g1; g2; game, member game player |> update_game win] else
        player
    else player
  | _ -> failwith "not a player"

let update_player_stats name money game win =
  let j = Yojson.Basic.from_file "stats.json" in
  let assoc = j |> member "Players" in
  let total = j |> member "Total games played" in
  match assoc with
  | `List lst ->
    let updated = List.map (update_if_match name money game win) lst in
    `Assoc ["Players", `List updated; "Total games played", total]
    |> Yojson.Basic.to_file "stats.json"
  | _ -> failwith "wrong json format"

(** [update_total_game_stats_h name assoc] adds 1 to the number of plays of
    the game with name [name] in [assoc]. *)
let update_total_game_stats_h name assoc =
  let update_if_match pair =
    match pair with
    | game, `Int n as data ->
      if game |> String.lowercase_ascii = name then game, `Int (n + 1)
      else data
    | _ -> failwith "wrong json format" in
  match assoc with
  | `Assoc lst -> List.map update_if_match lst
  | _ -> failwith "wrong json format"

let update_total_game_stats game =
  let j = Yojson.Basic.from_file "stats.json" in
  let players = j |> member "Players" in
  let total = j |> member "Total games played" in
  let new_total = update_total_game_stats_h game total in
  let new_j =
    `Assoc ["Players", players; "Total games played", `Assoc new_total] in
  Yojson.Basic.to_file "stats.json" new_j

let parse_tools_cmd str =
  let cmd = String.trim str |> String.lowercase_ascii in
  match cmd with
  | "color"
  | "text color"
  | "set color"
  | "set text color" -> Set_Color
  | "rules"
  | "view rules" -> View_Rules
  | "stats"
  | "view stats"
  | "view statistics" -> View_Statistics
  | "return to game"
  | "return"
  | "back" -> Return
  | _ -> Invalid

(** [parse_color clr] is the [ANSITerminal.color] of [clr].
    Print color is that of [player]. *)
let rec parse_color player str =
  let clr = str |> String.trim |> String.lowercase_ascii in
  match clr with
  | "black" -> ANSITerminal.Black
  | "red" -> Red
  | "green" -> Green
  | "yellow" -> Yellow
  | "blue" -> Blue
  | "magenta" -> Magenta
  | "cyan" -> Cyan
  | "white" -> White
  | _ -> print_string player.style "Not a color. Try again.\n";
    read_line () |> parse_color player

and set_color game player =
  let str = print_string player.style "Select a color: Black, Red, Green, \
                                       Yellow, Blue, Magenta, Cyan, White\n";
    read_line () in
  let clr = parse_color player str in
  player.style <- [Foreground clr];
  show_menu game player

and view_rules game player return =
  List.assoc game rules |> print_string player.style;
  print_endline "";
  if return then show_menu game player else ()



(** [view_my_stats j game player] displays the statistics for [player] in [j]
        during the game with name [game] *)
and view_my_stats j game player =
  let players = j |> member "Players" |> to_list in
  let me = players |> List.find
             (fun p -> p |> member "Name" |> to_string = player.name) in
  print_stats_player me player;
  view_stats game player


(** [view_all_stats j game player] displays the statistics in [j] for all
    players in the game with name [game] during [player]'s turn *)
and view_all_stats j game player =
  let players = j |> member "Players" |> to_list in
  List.iter (fun p -> print_stats_player p player) players;
  (str_of_total_stats j) ^ "\n" |>  print_string player.style;
  view_stats game player

(** [reset_stats game player] resets the statistics in the game with
    name [name] during [player]'s turn *)
and reset_stats game player =
  let default = {|{
  "Players": [],
  "Total games played": {
    "Blackjack": 0,
    "Poker": 0,
    "Baccarat": 0
  }
}|} in
  let () =
    let file = "stats.json" in
    let oc = open_out file in
    Printf.fprintf oc "%s" default; close_out oc; () in
  view_stats game player

(** [use_stats player] allows [player] to view all data
    and reset statistics, eventually able to return to the game with name
    [game]. *)
and view_stats game player =
  let j = Yojson.Basic.from_file "stats.json" in
  print_string player.style "Select: view my stats, view all stats, \
                             reset stats for all players, return to tools\n> ";
  let str = read_line () |> String.trim |> String.lowercase_ascii in
  match str with
  | "my stats"
  | "view my stats" -> view_my_stats j game player
  | "view all"
  | "all stats"
  | "view all stats" -> view_all_stats j game player
  | "reset"
  | "reset stats"
  | "reset stats for all"
  | "reset stats for all players" -> reset_stats game player
  | "return"
  | "tools"
  | "return to tools" -> show_menu game player
  | _ -> print_string player.style "Not a valid command. Try again.";
    view_stats game player

and show_menu game player =
  print_string player.style "Tools: set text color, view rules, view \
                             statistics, return to game.\n";
  print_string player.style input_prompt;
  let cmd = read_line () |> parse_tools_cmd in
  match cmd with
  | Set_Color -> set_color game player
  | View_Rules -> view_rules game player true
  | View_Statistics -> view_stats game player
  | Return -> ()
  | _ -> print_string player.style "Not an available tool.";
    show_menu game player