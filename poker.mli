open Deck

type hand =
  | HighCard of card list
  | Pair of rank * card list
  | TwoPairs of rank * rank * card list
  | ThreeOfAKind of rank * card list
  | Straight of card list
  | Flush of card list
  | FullHouse of rank * rank
  | FourOfAKind of rank * card list
  | StraightFlush of card list
  | RoyalFlush

(** The number of cards each player begins with. *)
val initial_cards : int

(** [hand_value cards] is the greatest possible hand value.
    The length of cards has to be 7. *)
val hand_value : card list -> hand

(** [cmp_hand c1 c2] compares the hand value of two card lists. Returns 1 if 
    hand value of c1 is greater than that of c2, -1 for less than, 0 for 
    equal. *)
val cmp_hand : card list -> card list -> int

