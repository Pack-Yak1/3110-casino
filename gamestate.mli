type player = Player.t

(** The abstract type representing a turn in a game. [name] is the name of the
    cardgame being played. [game_deck] is the main deck on the table from which
    players must draw. [turn] is the 0-index of the current player in 
    [players] to play. [player_num] is the number of players in the game. *)
type t = {
  name : string;
  mutable game_deck : Deck.t;
  player_num : int;
  turn : int;
  mutable players : player list;
  currency : string;
  num_decks : int
}

val default_game : t

val default_currency : string

(** [update_players n p s] is [s] with [player_num] equal to [n], and [players]
    equal to [p]. *)
val update_players : int -> player list -> t -> t

(** Determines whose turn it is, then prompts and takes a command. Recurses
    to automatically begin the next action. *)
val bj_turn : t -> t

(** [choose_game] is a game state with an empty deck, 1 player and game 
    name equal to that entered if it is a supported game. If not, the player is
    prompted to enter a valid game name again. *)
val choose_game : unit -> t

(** [shared_init s init_bet has_dealer starting_cards turn] is a game state 
    with number of decks, number of players, player names and initial bets 
    (iff [init_bet] is true) taken from user responses in StdIn. The state 
    initializes a dealer iff [has_dealer] is true, and deals each player 
    [starting_cards]. The game state returned is [s] mutated. *)
val shared_init : t -> bool -> bool -> int -> t

(** [play_round init_bet has_dealer starting_cards state turn] starts a round
    with values given *)
val play_round : bool -> bool -> int -> t -> (t -> t) -> unit