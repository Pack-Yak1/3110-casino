type suit = S | H | C | D

type rank = int

type card = (suit * rank)

type t = card list

exception InvalidRank of int

let lowest_rank = 2
let highest_rank = 14
let j_rank = 11
let q_rank = 12
let k_rank = 13
let a_rank = 14

let make_card (s : suit) (r : rank) : card = 
  if r > highest_rank || r < lowest_rank then raise(InvalidRank r) else (s, r)

let make_deck (lst : card list) : t = 
  lst

let suit_to_int suit = 
  match suit with
  | S -> 3
  | H -> 2
  | C -> 1
  | D -> 0

let suit ((s, r) : card) = 
  s

let rank ((s, r) : card) = 
  r

let of_card ((s, r) : card) : (suit * rank) = 
  (s, r)

let of_deck (deck : t) : card list = 
  deck

let cmp_suit ((s1, r1) : card) ((s2, r2) : card) : int = 
  compare (s1 |> suit_to_int) (s2 |> suit_to_int) 

let cmp_rank ((s1, r1) : card) ((s2, r2) : card) : int = 
  compare r1 r2

let cmp_card (c1 : card) (c2 : card) : int = 
  match cmp_rank c1 c2 with
  | 0 -> cmp_suit c1 c2
  | x -> x

let cmp_high_cards c1 c2 = 
  let l1 = List.map (fun c -> rank c) c1 in 
  let l2 = List.map (fun c -> rank c) c2 in 
  compare l1 l2

let deck_eq (d1 : t) (d2 : t) = 
  List.sort_uniq compare d1 = List.sort_uniq compare d2

let length (deck : t) = 
  List.length deck

(** Generate a list of integers [[low..high]] inclusive. *)
let rec generate_ranks low high = 
  if low = high then [high]
  else low :: generate_ranks (low + 1) high

(** Generate a deck consisting of cards with suit [suit] and rank for each
    rank in [ranks]. *)
let make_suit suit ranks = 
  List.map (make_card suit) ranks 

let std_deck () : t = 
  let ranks = generate_ranks lowest_rank highest_rank in
  make_suit S ranks
  |> List.append (make_suit H ranks) 
  |> List.append (make_suit C ranks) 
  |> List.append (make_suit D ranks) 

let sort deck = 
  List.sort cmp_card deck

let rev_sort deck =
  deck |> List.sort cmp_rank |> List.rev

let sort_suit deck =
  List.sort cmp_suit deck

let rec pick deck n = 
  match deck with
  | [] -> None
  | h :: t -> if n = 0 then Some h else pick t (n - 1)

let append (d : t) (c : card) : t = 
  c :: d

let remainder (d : t) : t = 
  match d with
  | [] -> failwith "Deck is empty, there is no remainder."
  | h :: t -> t

let rec return (cards : t) (n : int ) : t = 
  match n with
  | 0 -> []
  | n -> begin
      match cards with
      | [] -> failwith "not enough cards to draw"
      | h :: t -> (n - 1 |> return t |> append) h
    end

(** [bj_rank acc card] is the rank of the card unless it is a) a face card, in
    which case it is 10, or b) an ace, in which case it is 11, unless that 
    causes the player's score to exceed 21, in which case it returns 1. *)
let bj_rank acc ((s, r) : card) = 
  if r < a_rank then r |> min 10
  else if acc > 10 then 1
  else 11

let bj_score deck = 
  let d = sort deck in
  let sum = List.fold_left (fun acc card -> acc + bj_rank acc card) 0 d in
  let nat_bj = length deck = 2 && sum = 21 in
  if nat_bj then -1 else sum

let rec rank_of_pair = function
  | [] | [_] -> None
  | h1 :: h2 :: t -> 
    if cmp_rank h1 h2 = 0 then Some (rank h1)
    else rank_of_pair (h2 :: t)

let rank_filter r cards : t =
  List.filter (fun (s, n) -> n <> r) cards

let flush deck = 
  match sort_suit deck with
  | [c1; c2; c3; c4; c5; c6; c7] as c -> 
    if cmp_suit c1 c7 = 0 then c
    else if cmp_suit c1 c6 = 0
    then [c1; c2; c3; c4; c5; c6]
    else if cmp_suit c1 c5 = 0 
    then rev_sort [c1; c2; c3; c4; c5]
    else if cmp_suit c2 c7 = 0
    then [c2; c3; c4; c5; c6; c7]
    else if cmp_suit c2 c6 = 0 
    then rev_sort [c2; c3; c4; c5; c6]
    else if cmp_suit c3 c7 = 0 
    then rev_sort [c3; c4; c5; c6; c7]
    else []
  | _ -> [] 

let rec check_straight = function
  | [] | [_] -> true 
  | h1 :: h2 :: t -> if rank h1 - rank h2 = 1 
    then check_straight (h2 :: t) else false

let remove_dup_rank cards : t = 
  let rec helper = function
    | [] -> []
    | (s, r) :: t -> 
      (s, r) :: helper (rank_filter r t)
  in helper cards

let straight cards = 
  let cards = cards |> rev_sort |> remove_dup_rank in 
  match cards with 
  | [c1; c2; c3; c4; c5; c6; c7] -> 
    if check_straight [c1; c2; c3; c4; c5]
    then [c1; c2; c3; c4; c5]
    else if check_straight [c2; c3; c4; c5; c6]
    then [c2; c3; c4; c5; c6]
    else if check_straight [c3; c4; c5; c6; c7]
    then [c3; c4; c5; c6; c7]
    else []
  | [c1; c2; c3; c4; c5; c6] -> 
    if check_straight [c1; c2; c3; c4; c5]
    then [c1; c2; c3; c4; c5]
    else if check_straight [c2; c3; c4; c5; c6]
    then [c2; c3; c4; c5; c6]
    else []
  | [c1; c2; c3; c4; c5] as c -> 
    if check_straight c then c else []
  | _ -> []

let straight_flush cards = 
  let fl = flush cards in
  straight fl 

let concat d1 d2 = 
  List.rev_append d1 d2

let stable_concat d1 d2 = 
  List.append d1 d2

let empty_deck () : t = 
  []

let is_empty = function
  | [] -> true
  | _ -> false 

let rec n_std_decks_helper (n : int) (acc : t) : t = 
  match n with
  | 0 -> acc
  | n -> std_deck () |> concat acc |> n_std_decks_helper (n - 1)

let n_std_decks (n : int) : t = 
  empty_deck () |> n_std_decks_helper n

let shuffle (cards : t) : t = 
  Random.self_init ();
  let random = List.map (fun n -> (Random.bits(), n)) cards in 
  let sorted = List.sort compare random in 
  List.map (fun (a, b) -> b) sorted

let string_of_suit = function
  | S -> "♤"
  | H -> "♡"
  | C -> "♧"
  | D -> "♢"

let string_of_rank n = 
  if lowest_rank <= n && n <= 10 then string_of_int n
  else if n = j_rank then "J"
  else if n = q_rank then "Q"
  else if n = k_rank then "K"
  else if n = a_rank then "A"
  else raise (InvalidRank n)

let string_of_card ((s, r) : card) = 
  "(" ^ string_of_suit s ^ ", " ^ string_of_rank r ^ ")"

let rec string_of_deck_helper deck acc = 
  match deck with
  | [] -> acc
  | [h] -> acc ^ string_of_card h
  | h1 :: (h2 :: t as t') -> begin
      string_of_deck_helper t' (acc ^ string_of_card h1 ^ "; ")
    end

let string_of_deck deck = 
  let d = sort deck in
  "[" ^ string_of_deck_helper d "" ^ "]"