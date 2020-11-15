(** The type [bj_command] represents a player command for a game of blackjack*)
type bj_command =
  | Hit
  | Stand
  | Double
  | Quit
  | Tools

(** The type [p_command] represents a player command for a game of poker*)
type p_command = 
  | Check
  | Raise
  | Call
  | Fold
  | Remind
  | Quit
  | Tools

(** Raised when the command is invalid. *)
exception Invalid_command

(** [parse_bj str] parses a player's input into a blackjack command. Case does 
    not matter. 
    Raises: [InvalidCommand] if the command is invalid. *)
val parse_bj : string -> bj_command

(** [parse_p str] parses a player's input into a poker command. Case does not 
    matter. 
    Raises: [InvalidCommand] if the command is invalid. *)
val parse_p : string -> p_command