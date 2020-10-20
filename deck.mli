(** The type of the suit of a card. *)
type suit = S | H | C | D

(** The type of rank of a card. The rank of jack is 11, rank of queen is 12, 
    rank of king is 13, rank of ace is 14. Rank must be in [[2..14]]. *)
type rank = int

(** The abstract type representing a card in a card game.
    Requires: suit and rank have valid values. *)
type card

(** The abstract type representing a deck of cards. *)
type t

exception InvalidRank of int

(** [make_card suit rank] is a card with suit [suit] and rank [rank]. *)
val make_card : suit -> rank -> card

(** [make_deck lst] is a deck comprising all cards in card list [lst]. *)
val make_deck : card list -> t

(** [std_deck ()] is a standard deck of 52 cards. *)
val std_deck : unit -> t

(** [length deck] is the number of cards in [deck]. *)
val length : t -> int

(** [cmp_suit c1 c2] is a positive integer if the suit of [c1] is greater than
    [c2], a negative integer if the suit of [c2] is greater than [c1], or zero 
    if the suits of [c1] and [c2] are equal. 
    Requires: [c1] and [c2] are valid cards. *)
val cmp_suit : card -> card -> int

(** [cmp_rank c1 c2] is a positive integer if the rank of [c1] is greater than
    [c2], a negative integer if the rank of [c2] is greater than [c1], or zero 
    if the ranks of [c1] and [c2] are equal. 
    Requires: [c1] and [c2] are valid cards. *)
val cmp_rank : card -> card -> int

(** [cmp_card c1 c2] is a positive integer if [c1] is greater than [c2], a 
    negative integer if [c2] is greater than [c1], or zero if [c1] and [c2] 
    are equal. Cards are compared first by rank, then by suit.
    Requires: [c1] and [c2] are valid cards. *)
val cmp_card : card -> card -> int

(** [deck_eq d1 d2] is true if [d1] and [d2] contain the same cards. 
    Otherwise it is false.  *)
val deck_eq : t -> t -> bool

(** [sort deck] is [deck] sorted in ascending order by rank, then by suit in
    the case of a rank tie. *)
val sort : t -> t

(** [pick deck n] is the 0-index n-th card option of [deck]. Returns None if
    [n] is greater than or equal to the number of cards in [deck]. *)
val pick : t -> int -> card option

(** [bj_score deck] is the integer score of [deck] in a traditional game of 
    blackjack. *)
val bj_score : t -> int

(** [string_of_card card] is a 2-character string representing [card]. The
    first character is the suit, and the second character is the rank. *)
val string_of_card : card -> string

(** [string_of_deck deck] is a string of the cards comprising [deck] in 
    ascending order. *)
val string_of_deck : t -> string