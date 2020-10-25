(* open Main *)

(*TODO: fix type of player, access from Main module
  Integrate tools in Main *)
type player = {
  name : string;
  mutable hand : Deck.t;
  mutable bet : int;
  mutable money : int;
  mutable style : ANSITerminal.style list
}

type color = ANSITerminal.color

type tools_command =
  | Set_Color
  | View_Rules
  | View_Statistics
  | Quit
  | Invalid

(** Printed before user input *)
let input_prompt = "> "

let parse_tools_cmd str =
  let cmd = String.trim str |> String.lowercase_ascii in
  match cmd with
  | "set color" -> Set_Color
  | "view rules" -> View_Rules
  | "view statistics"
  | "view stats" -> View_Statistics
  | "quit" -> Quit
  | _ -> Invalid

(** [parse_color clr] is the [ANSITerminal.color] of [clr].
    If [clr] is not a valid color, select default color. *)
let parse_color str =
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
  | _ -> print_string "Default color has been selected."; Default

let set_color player =
  let str = read_line () in
  let clr = parse_color str in
  player.style <- [Foreground clr]

let view_rules () =
  (* Stdlib.print_string "> ";
     let name = read_line () in
     Stdlib.print_string "" *)
  failwith "Unimplemented"

(*TODO: open statistics file and find player's data, then report all data on
  prompt *)
let view_statistics player =
  let _ = failwith "Unimplemented" in ()

let rec show_menu player =
  print_endline "Tools: set text color, view rules, view statistics, quit. \
                 What would you like to do?";
  print_endline input_prompt;
  let cmd = read_line () |> parse_tools_cmd in
  match cmd with
  | Set_Color -> set_color player
  | View_Rules (*unimplemented: print rules*)
  | View_Statistics -> view_statistics player
  | Quit (*unimplemented: return to game*)
  | _ -> print_endline "Not an available tool."; show_menu player