open Deck

let initial_cards = 2

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

let hand_score = function
  | HighCard _ -> 1
  | Pair _ -> 2
  | TwoPairs _-> 3
  | ThreeOfAKind _ -> 4
  | Straight _-> 5
  | Flush _ -> 6
  | FullHouse _ -> 7
  | FourOfAKind _-> 8
  | StraightFlush _ -> 9
  | RoyalFlush -> 10

let rev_sort lst =
  lst |> List.sort cmp_rank |> List.rev

(** [rank_of_pair cards] returns the rank of pair in [cards]. *)
let rec rank_of_pair = function
  | [] | [_] -> None
  | h1 :: h2 :: t -> 
    if cmp_rank h1 h2 = 0 then Some (rank h1)
    else rank_of_pair (h2 :: t)

let rec is_pair cards = match cards with
  | [] | [_] -> HighCard cards
  | h :: t -> let r = rank_of_pair (h :: t) in 
    match r with
    | None -> is_pair t
    | Some n -> 
      let rem = List.filter (fun x -> rank x <> n) cards in
      Pair (n, rem)

let max_card r lst = 
  let l = List.filter (fun x -> rank x <> r) lst in
  match l with
  | [] -> []
  | h :: t -> [h]

let first_five lst =
  let rec helper n = function
    | [] -> []
    | h :: t -> 
      if n = 0 then [] else h :: helper (n - 1) t
  in helper 5 lst

let is_full_house cards =
  let cards = rev_sort cards in
  match is_pair cards with
  | Pair (r, [c1; c2; c3]) -> FourOfAKind (r, [c1])
  | Pair (r, [c1; c2; c3; c4]) -> begin
      let rem = [c1; c2; c3; c4] in 
      match is_pair rem with 
      | Pair (r', []) -> FourOfAKind (r', [List.hd cards])
      | Pair (r', [c]) -> FullHouse (r, r')
      | Pair (r', [c5; c6]) -> FullHouse (r, r')
      |  _ -> ThreeOfAKind (r, [c1; c2])
    end
  | Pair (r, [c1; c2; c3; c4; c5]) -> begin 
      let rem = [c1; c2; c3; c4; c5] in 
      match is_pair rem with
      | Pair (r', [c]) -> FourOfAKind (r', max_card r' cards)
      | Pair (r', [c6; c7]) -> FullHouse (r', r)
      | Pair (r', [c6; c7; c8]) -> begin 
          match is_pair [c6; c7; c7] with 
          | Pair (r'', []) -> FullHouse (r'', r)
          | _ -> TwoPairs (r, r', [c6]) 
        end
      | _ -> Pair (r, [c1; c2; c3]) 
    end
  | _ -> HighCard (first_five cards) 

let flush cards = 
  let cards = List.sort cmp_suit cards in 
  match cards with
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

let remove_dup_rank cards = 
  let rec helper = function
    | [] -> []
    | h :: t -> 
      h :: helper (List.filter (fun c -> rank c <> rank h) t)
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

let rec same_suit c = function
  | [] -> false
  | h :: t -> if cmp_suit h c = 0 then true 
    else same_suit c t

let straight_flush cards = 
  let fl = flush cards in
  let st = straight fl in
  match st with
  | [] -> []
  | h :: t -> st

let is_straight_flush cards = 
  let st = straight cards in
  let fl = flush cards in
  let fl' = first_five fl in
  let st_fl = straight_flush cards in
  match (st, fl') with
  | (h1 :: t1), (h2 :: t2) -> begin 
      match st_fl with 
      | [] -> Flush fl'
      | h :: t -> 
        if rank h = 14 then RoyalFlush 
        else StraightFlush fl' 
    end 
  | [], (h :: t) -> Flush fl
  | (h :: t), [] -> Straight st
  | [], [] -> let cards = rev_sort cards in 
    HighCard (first_five cards)

let cmp_type t1 t2 =
  compare (hand_score t1) (hand_score t2)

let hand_value cards = 
  let t1 = is_straight_flush cards in 
  let t2 = is_full_house cards  in
  if cmp_type t1 t2 > 0 then t1 else t2

let get_cards = function
  | HighCard c -> c
  | Pair (r, c) -> c
  | TwoPairs (r1, r2, c) -> c
  | ThreeOfAKind (r, c) -> c
  | Straight c -> c
  | Flush c -> c
  | FourOfAKind (r, c) -> c 
  | StraightFlush c -> c
  | _ -> []

let get_rank = function
  | Pair (r, c) -> r
  | TwoPairs (r1, r2, c) -> r1 
  | ThreeOfAKind (r, c) -> r
  | FullHouse (r1, r2) -> r1
  | FourOfAKind (r, c) -> r
  | _ -> 0

let get_sec_rank = function
  | TwoPairs (r1, r2, c) -> r2
  | FullHouse (r1, r2) -> r2
  | _ -> 0

let cmp_high_card h1 h2 = 
  let l1 = h1 |> get_cards |> List.map (fun c -> rank c) in 
  let l2 = h2 |> get_cards |> List.map (fun c -> rank c) in 
  compare l1 l2

let cmp_hand c1 c2 = 
  let t1 = hand_value c1 in 
  let t2 = hand_value c2 in 
  let c = cmp_type t1 t2 in 
  if c = 0 then
    let r1 = get_rank t1 in 
    let r2 = get_rank t2 in
    if r1 = r2 then 
      let r3 = get_sec_rank t1 in 
      let r4 = get_sec_rank t2 in 
      if r3 = r4 then cmp_high_card t1 t2 
      else compare r3 r4 
    else compare r1 r2
  else c