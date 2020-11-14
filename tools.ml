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
let blackjack_rules = "Rules\n Hit: Take another card from the dealer.\n \
                       Stand: Take no more cards.\n Double down: Double your \
                       bet and commit to standing after one more hit.\n Quit: \
                       Quit the game.\n Tools: View or edit settings and \
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
let bridge_rules = "To be implemented"

let rules = [
  ("blackjack", blackjack_rules);
  ("poker", poker_rules)
]

let j = Yojson.Basic.from_file "stats.json"

let parse_tools_cmd str =
  let cmd = String.trim str |> String.lowercase_ascii in
  match cmd with
  | "set color"
  | "set text color" -> Set_Color
  | "view rules" -> View_Rules
  | "view statistics"
  | "view stats" -> View_Statistics
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

(** [print_stats game j_player player] prints the statistisc for [j_player]
    during [player]'s turn in the game with name [game] *)
and print_stats game j_player player =
  let name = j_player |> member "name" |> to_string in
  let money = j_player |> member "money" |> to_string in
  let plays = j_player |> member "plays" |> to_int in
  "Name: " ^ name ^
  "\nMoney: " ^ money ^
  "\nPlays: " ^ string_of_int plays ^ "\n" |> print_string player.style;

  (** [view_my_stats game player] displays the statistics for [player]
      during the game with name [game] *)
and view_my_stats game player =
  let players = j |> member "players" |> to_list in
  let me = players |> List.find
             (fun p -> p |> member "name" |> to_string = player.name) in
  print_stats game me player;
  use_stats game player

(** [view_all_stats game player] displays the statistics for all players
    in the game with name [game] during [player]'s turn *)
and view_all_stats game player =
  let players = j |> member "players" |> to_list in
  List.iter (fun p -> print_stats game p player) players;
  print_string player.style "Total games played: ";
  let t_plays = j |> member "Total games played" |> to_int |> string_of_int in
  print_string player.style (t_plays ^ "\n");
  use_stats game player

(** [reset_stats game player] resets the statistics in the game with
    name [name] during [player]'s turn *)
and reset_stats game player =
  let default = {|{
  "players": [
    {
      "name": "default_player",
      "money": "10000 USD",
      "plays": 20000
    }
  ],
  "Total plays": 0
}|} in
  let () =
    let file = "stats.json" in
    let oc = open_out file in
    Printf.fprintf oc "%s" default; close_out oc; () in
  use_stats game player

(** [use_stats player] allows [player] to view all data
    and reset statistics, eventually able to return to the game with name
    [game]. *)
and use_stats game player =
  print_string player.style "Select: view my stats, view all stats, \
                             reset stats, return to tools\n> ";
  let str = read_line () |> String.trim |> String.lowercase_ascii in
  match str with
  | "my stats"
  | "view my stats" -> view_my_stats game player
  | "view all"
  | "all stats"
  | "view all stats" -> view_all_stats game player
  | "reset"
  | "reset stats" -> reset_stats game player
  | "return"
  | "tools"
  | "return to tools" -> show_menu game player
  | _ -> print_endline "Not a valid command. Try again.";
    use_stats game player

(*TODO: open statistics file and find player's data, then report all data on
  prompt. plays, wins, money
  parse json?*)
and view_statistics game player =
  use_stats game player;
  show_menu game player

and show_menu game player =
  print_string player.style "Tools: set text color, view rules, view \
                             statistics, return to game.\n";
  print_string player.style input_prompt;
  let cmd = read_line () |> parse_tools_cmd in
  match cmd with
  | Set_Color -> set_color game player
  | View_Rules -> view_rules game player true
  | View_Statistics -> view_statistics game player
  | Return -> ()
  | _ -> print_endline "Not an available tool."; show_menu game player