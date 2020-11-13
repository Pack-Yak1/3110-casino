(** The abstract type representing a player. [name] is the name of the player,
    [hand] is the player's deck, [money] is the amount of money the player has 
    remaining, [style] is the ANSITerminal display style. *)
type t = {
  name : string;
  mutable hand : Deck.t;
  mutable bet : int;
  mutable money : int;
  mutable in_game : bool;
  mutable style : ANSITerminal.style list
}

(** By default, all players have this amount when generated, except the 
    dealer. *)
val default_allowance : int

val default_player : t

val dealer : t

(** [create_players n] is a player list with [n] players.
    The name of each player is taken from std input. *)
val create_players : int -> t list

(** [print_score curr p] prints the score of player [p] in unit [curr] *)
val print_score : string -> t -> unit