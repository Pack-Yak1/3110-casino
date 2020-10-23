(** The type [command] represents a player command。*)
type command =
  | Hit
  | Stand
  | Double
  | Quit

(** Raised when the command is invalid. *)
exception Invalid_command

(** [parse str] parses a player's input into a command. Case does not matter. 
    Raises: [InvalidCommand] if the command is invalid. *)
val parse : string -> command