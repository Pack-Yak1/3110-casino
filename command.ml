type command =
  | Hit 
  | Stand
  | Split

exception Invalid_command

let parse str = 
  let c = str |> String.trim |> String.lowercase_ascii in 
  match c with 
  | "hit" -> Hit
  | "stand" -> Stand
  | "split" -> Split
  | _ -> raise Invalid_command