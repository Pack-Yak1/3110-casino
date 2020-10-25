(** Represents a player whose tools are accessed. *)
type player

(** Represents a text color. *)
type color

(** Represents a valid command in Tools. *)
type tools_command =
  | Set_Color
  | View_Rules
  | View_Statistics
  | Quit
  | Invalid

(** [parse_tools_cmd str] is the [tools_command] of [str]. *)
val parse_tools_cmd : string -> tools_command

(** Opens the tools menu for a [player]. *)
val show_menu : player -> unit

(** Sets the color of [player]'s in-game text when prompted. *)
val set_color : player -> unit

(** Displays rules for game entered. *)
val view_rules : unit -> unit

(** Displays statistics for player for past games. *)
val view_statistics : player -> unit