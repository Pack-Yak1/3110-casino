val input_prompt : string

(** [get_gamemode ()] is a game name from player input, if the player inputs 
    a supported game name. If not, the player is prompted to enter a valid 
    game name again. *)
val get_gamemode : unit -> string

(** [choose_num_geq_1_leq_n initial_prompt invalid_msg cap exists_limit]
    prompts the player to enter an int greater than 0 (and possibly less than
    or equal to [cap], depending on [exists_limit]) by printing
    [initial_prompt]. If the input is not greater than 0, or if the input is
    greater than [cap] and [exists_limit] is true, it prints [invalid_msg]. *)
val choose_num_geq_1_leq_n : string -> string -> int -> bool -> int

(** [choose_bet ()] is a Baccarat outcome, banker, player, or tie, the player 
    wants to bet on. *)
val choose_bet : unit -> Baccarat.outcome

(** [update_currency ()] prompts for a currency to be used in casino and 
    returns the user's choice. *)
val update_currency : string -> string

(** Prompts for a yes or no response to whether another round is desired. *)
val yes_or_no : string -> bool