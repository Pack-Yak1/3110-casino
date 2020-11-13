(** Represents a player whose tools are accessed. *)
type player = Player.t

(** Represents a text color. *)
type color

(** Represents a valid command in Tools. *)
type tools_command =
  | Set_Color
  | View_Rules
  | View_Statistics
  | Return
  | Invalid

(** [parse_tools_cmd] is the [tools_command] of input text. *)
val parse_tools_cmd : string -> tools_command

(** Opens the tools menu for a [player] in game with name [game]. *)
val show_menu : string -> player -> unit

(** Sets the color of [player]'s in-game text in all games when prompted,
    then returns to the game with name [game]. *)
val set_color : string -> player -> unit

(** Displays rules for [game] for [player]. Returns to Tools menu
    iff [return]*)
val view_rules : string -> player -> bool -> unit

(** Displays statistics for [player] for past games, then returns to
    the game with name [game]. *)
val view_statistics : string -> player -> unit