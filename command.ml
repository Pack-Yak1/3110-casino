type bj_command =
  | Hit 
  | Stand
  | Double
  | Quit
  | Tools

type p_command = 
  | Check
  | Raise
  | Call
  | Fold
  | Quit
  | Tools

exception Invalid_command

let parse_bj str = 
  let c = str |> String.trim |> String.lowercase_ascii in 
  match c with 
  | "hit" -> Hit
  | "stand" -> Stand
  | "double down" -> Double
  | "quit" -> Quit
  | "tools" -> Tools
  | _ -> raise Invalid_command

let parse_p str = 
  let c = str |> String.trim |> String.lowercase_ascii in 
  match c with 
  | "check" -> Check
  | "raise" -> Raise
  | "call" -> Call
  | "fold" -> Fold
  | "quit" -> Quit
  | "tools" -> Tools
  | _ -> raise Invalid_command