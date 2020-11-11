(** The abstract type representing a player. [name] is the name of the player,
    [hand] is the player's deck, [money] is the amount of money the player has 
    remaining, [style] is the ANSITerminal display style. *)
type t = {
  name : string;
  mutable hand : Deck.t;
  mutable bet : int;
  mutable money : int;
  mutable style : ANSITerminal.style list
}

(** By default, all players have this amount when generated, except the 
    dealer. *)
val default_allowance : int

val default_player : t

val dealer : t