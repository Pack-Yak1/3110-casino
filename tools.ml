open Player
open ANSITerminal

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
(** TODO: Finish typing rules *)
let poker_rules = "Rules\n Check: Decline to bet. You keep your hand but do \
                   not open.\n 
                   Stand: Take no more cards.\n Double down: Double your \
                   bet and commit to standing after one more hit.\n Quit: \
                   Quit the game.\n Tools: View or edit settings and \
                   rules."
let bridge_rules = "To be implemented"

let rules = [
  ("blackjack", blackjack_rules);
  ("poker", poker_rules)
]

let parse_tools_cmd str =
  let cmd = String.trim str |> String.lowercase_ascii in
  match cmd with
  | "set color"
  | "set text color" -> Set_Color
  | "view rules" -> View_Rules
  | "view statistics"
  | "view stats" -> View_Statistics
  | "return to game"
  | "return" -> Return
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

let set_color player =
  let str = print_string player.style "Select a color: Black, Red, Green, \
                                       Yellow, Blue, Magenta, Cyan, White\n";
    read_line () in
  let clr = parse_color player str in
  player.style <- [Foreground clr]

let view_rules player name =
  List.assoc name rules |> print_string player.style;
  print_endline "\n"

(*TODO: open statistics file and find player's data, then report all data on
  prompt. plays, quits, wins, money
  parse json?*)
let view_statistics player =
  let _ = print_endline "" in ()

let rec show_menu game player =
  print_string player.style "Tools: set text color, view rules, view \
                             statistics, return to game.\n";
  print_string player.style input_prompt;
  let cmd = read_line () |> parse_tools_cmd in
  match cmd with
  | Set_Color -> set_color player
  | View_Rules -> view_rules player game
  | View_Statistics -> view_statistics player
  | Return -> ()
  | _ -> print_endline "Not an available tool."; show_menu game player