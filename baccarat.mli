type outcome =
  | Banker
  | Player
  | Tie

(** The number of cards every player is dealt at the start of a game. *)
val initial_cards : int

(** [init_bet] is true iff players are allowed to case bets before cards are 
    dealt *)
val init_bet : bool

(** [has_dealer] is true iff the game has a dealer controlled by the computer.*)
val has_dealer : bool