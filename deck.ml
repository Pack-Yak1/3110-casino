type suit = S | H | C | D

type rank = int

type card = (suit * rank)

type t = card list

exception InvalidRank of int

let make_card (s : suit) (r : rank) : card = 
  if r > 14 || r < 2 then raise(InvalidRank r) else (s, r)

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
  let ranks = generate_ranks 2 14 in
  make_suit S ranks
  |> List.append (make_suit H ranks) 
  |> List.append (make_suit C ranks) 
  |> List.append (make_suit D ranks) 

let sort deck = 
  List.sort cmp_card deck

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
  | n -> List.hd cards :: return (List.tl cards) (n - 1)

(** [bj_rank acc card] is the rank of the card unless it is a) a face card, in
    which case it is 10, or b) an ace, in which case it is 11, unless that 
    causes the player's score to exceed 21, in which case it returns 1. *)
let bj_rank acc ((s, r) : card) = 
  if r < 14 then r |> min 10
  else if acc > 10 then 1
  else 11

let bj_score deck = 
  if length deck = 2 && 
     List.fold_left (fun acc card -> acc + bj_rank 0 card) 0 deck = 21 then -1
  else let d = sort deck in
    List.fold_left (fun acc card -> card |> bj_rank acc |> ( + ) acc) 0 d

let concat d1 d2 = 
  List.rev_append d1 d2

let stable_concat d1 d2 = 
  List.append d1 d2

let rec n_std_decks (n : int) : t = 
  match n with
  | 0 -> []
  | n -> n - 1 |> n_std_decks |> concat (std_deck ())

let empty_deck () : t = 
  []

let shuffle (cards : t) : t = 
  let random = List.map (fun n -> (Random.bits(), n)) cards in 
  let sorted = List.sort compare random in 
  List.map (fun (a, b) -> b) sorted

let string_of_suit = function
  | S -> "S"
  | H -> "H"
  | C -> "C"
  | D -> "D"

let string_of_rank n = 
  if 2 <= n && n <= 10 then string_of_int n
  else if n = 11 then "J"
  else if n = 12 then "Q"
  else if n = 13 then "K"
  else if n = 14 then "A"
  else raise (InvalidRank n)

let string_of_card ((s, r) : card) = 
  "(" ^ string_of_suit s ^ ", " ^ string_of_rank r ^ ")"

(** Implementation taken from pp_lst from test file of A2. *)
let string_of_deck lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ string_of_card h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (string_of_card h1) ^ "; ") t'
    in loop 0 "" (sort lst)
  in "[" ^ pp_elts lst ^ "]"