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

(** [n_std_decks n] is a deck consisting of [n] standard 52-card decks. *)
val n_std_decks : int -> t

(** [empty_deck ()] is a deck containing no cards. *)
val empty_deck : unit -> t

(** [is_empty deck] is true if [deck] is empty, false otherwise. *)
val is_empty : t -> bool

(** [shuffle cards] is the deck [cards] sorted in random order. *)
val shuffle : t -> t

(** [length deck] is the number of cards in [deck]. *)
val length : t -> int

(** [suit card] is the suit of [card]. *)
val suit : card -> suit

(** [rank card] is the rank of [card]. 
    Raises: InvalidRank if rank of [card] is not in [[2..14]]. *)
val rank : card -> rank

(** [of_card card] is the pair containing the suit and rank of [card]. *)
val of_card : card -> (suit * rank)

(** [of_deck deck] is the list of cards in [deck]. *)
val of_deck : t -> card list

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

(** [cmp_high_cards d1 d2] is a positive integer if the [d1] has greater rank of
    card than [c2], a negative integer if [c2] has greater ranl than [c1], or 
    zero if [c1] and [c2] have same cards. *)
val cmp_high_cards : t -> t -> int

(** [deck_eq d1 d2] is true if [d1] and [d2] contain the same cards. 
    Otherwise it is false.  *)
val deck_eq : t -> t -> bool

(** [sort deck] is [deck] sorted in ascending order by rank, then by suit in
    the case of a rank tie. *)
val sort : t -> t

(** [rev_sort deck] is [deck] sorted in descending order of rank. *)
val rev_sort : t -> t

(** [pick deck n] is the 0-index n-th card option of [deck]. Returns None if
    [n] is greater than or equal to the number of cards in [deck]. *)
val pick : t -> int -> card option

(** [return n cards] is the first [n] cards in [cards].
    Requires: n is less than or equal to the number of cards in [cards]. *)
val return : t -> int -> t

(** [append d c] is [d] with [c] appended to it at the front. *)
val append : t -> card -> t

(** [remainder d] is [d] with the top card removed. *)
val remainder : t -> t

(** [bj_score deck] is the integer score of [deck] in a traditional game of 
    blackjack. If the deck is a natural blackjack (exactly one Ace and one face
    card), then [bj_score deck] is -1 to signify its unique value. *)
val bj_score : t -> int

(** [rank_of_pair cards] is the rank of first pair in [cards]. Return None if
    there is no pair in [cards] 
    Requires: [cards] must be sorted on rank. *)
val rank_of_pair : t -> rank option

(** [rank_filter r cards] is [cards] without card with rank [r]. *)
val rank_filter : rank -> t -> t

(** [flush cards] is part of [cards] that has five to seven cards with the
    same suit.
    Requires: [cards] must contain exactly seven cards. *)
val flush : t -> t

(** [straigth cards] is five cards in [cards] with greatest consecutive rank 
    value.
    Requires: [cards] must contain exactly seven cards. *)
val straight : t -> t

(** [straigth cards] is cards in [cards] that is both flush and straight.
    Requires: [cards] must contain exactly seven cards. *)
val straight_flush : t -> t

(** [concat d1 d2] is a deck containing all cards in d1 and d2. Repeated cards
    are allowed. The order of cards is not preserved. *)
val concat : t -> t -> t

(** [stable_concat d1 d2] is a deck containing all cards in d1 and d2. 
    Repeated cards are allowed. The order of cards is preserved, unlike in 
    [concat d1 d2]. *)
val stable_concat : t -> t -> t

(** [string_of_card card] is a 2-character string representing [card]. The
    first character is the suit, and the second character is the rank. *)
val string_of_card : card -> string

(** [string_of_deck deck] is a string of the cards comprising [deck] in 
    ascending order. *)
val string_of_deck : t -> string