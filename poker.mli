open Deck

type hand =
  | HighCard of Deck.t
  | Pair of rank * Deck.t
  | TwoPairs of rank * rank * Deck.t
  | ThreeOfAKind of rank * Deck.t
  | Straight of Deck.t
  | Flush of Deck.t
  | FullHouse of rank * rank
  | FourOfAKind of rank * Deck.t
  | StraightFlush of Deck.t
  | RoyalFlush

(** The number of cards each player begins with. *)
val initial_cards : int

(** [init_bet] is true iff players are allowed to case bets before cards are 
    dealt *)
val init_bet : bool

(** [has_dealer] is true iff the game has a dealer controlled by the computer.
*)
val has_dealer : bool

(** [hand_value deck] is the greatest possible hand value.
    Requires: the length of deck must be 7. *)
val hand_value : Deck.t -> hand

(** [cmp_hand d1 d2] compares the hand value of two decks. Returns 1 if 
    hand value of d1 is greater than that of d2, -1 for less than, 0 for 
    equal. *)
val cmp_hand : Deck.t -> Deck.t -> int

(** [string_of_hand hand] is a string of result of Poker. *)
val string_of_hand : hand -> string