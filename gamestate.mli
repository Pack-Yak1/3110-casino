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
  small_blind : int;
  num_decks : int;
  mutable ba_players : player list;
}

(** The abstract type representing meta game information for a given gamemode,
    which includes whether you are allowed to bet before cards are dealt, and
    if there exists a dealer, and the number of cards each player is dealt 
    initially. It also contains the appropriate function for executing game
    turns for the game mode. *)
type game_info = {
  init_bet : bool;
  has_dealer : bool;
  initial_cards : int;
  engine : t -> t;
}

val default_game : t

val default_currency : string

(** [update_players n p s] is [s] with [player_num] equal to [n], and [players]
    equal to [p]. *)
val update_players : int -> player list -> t -> t

(** Plays a game of poker and returns a game_state with the results of the
    game applieed. *)
val poker_turn : t -> t

(** Determines whose turn it is, then prompts and takes a command. Recurses
    to automatically begin the next action. Returns a game_state with the 
    results of the blackjack game applied. *)
val bj_turn : t -> t

(** Determines whose turn it is, then prompts and takes a command. Recurses
    to automatically begin the next action. Returns a game_state with the 
    results of the baccarat game applied. *)
val ba_turn : t -> t

(** [game_constructor state] prompts the player to select a gamemode and sets 
    the metadata of state to match the game mode desired. *)
val game_constructor : t -> unit