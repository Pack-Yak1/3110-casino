type player = Player.t

(** The abstract type representing a turn in a game. [name] is the name of the
    cardgame being played. [game_deck] is the main deck on the table from which
    players must draw. [turn] is the 0-index of the turn in the game, which
    increments with each player's turn (skipped players' turns are counted).
    [player_num] is the number of players in the game. *)
type t = {
  name : string;
  mutable game_deck : Deck.t;
  mutable flop : player;
  mutable player_num : int;
  turn : int;
  mutable players : player list;
  currency : string;
  mutable small_blind : int;
  num_decks : int;
  mutable ba_players : player list;
}

val default_game : t

(** [update_players n p s] is [s] with [player_num] equal to [n], and [players]
    equal to [p]. *)
val update_players : int -> player list -> t -> t

(** [game_constructor state] prompts the player to select a gamemode and sets 
    the metadata of state to match the game mode desired. *)
val game_constructor : t -> unit