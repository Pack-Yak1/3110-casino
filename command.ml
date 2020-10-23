type command =
  | Hit 
  | Stand
  | Double
  | Quit

exception Invalid_command

let parse str = 
  let c = str |> String.trim |> String.lowercase_ascii in 
  match c with 
  | "hit" -> Hit
  | "stand" -> Stand
  | "double down" -> Double
  | "quit" -> Quit
  | _ -> raise Invalid_command