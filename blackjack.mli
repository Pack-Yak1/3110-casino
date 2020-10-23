type outcome = 
  | DealerBust
  | Match of int

(** The number of cards every player is dealt at the start of a game. *)
val initial_cards : int

(** [win_check player_score outcome] is true if the player would win a game
    with [outcome], otherwise it is false. *)
val win_check : int -> outcome -> bool