open Deck

let initial_cards = 2

let init_bet = false

let has_dealer = false

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

let rec is_pair deck = match length deck with
  | 0 | 1 -> HighCard deck
  | _ -> let r = rank_of_pair deck in 
    match r with
    | None -> is_pair (remainder deck)
    | Some n -> Pair (n, rank_filter n deck)

let house_helper r rem cards = function
  | Pair (r', rem') when length rem' = 1 -> 
    FourOfAKind (r', return (rank_filter r' cards) 1)
  | Pair (r', rem') when length rem' = 2 -> 
    FullHouse (r', r)
  | Pair (r', rem') when length rem' = 3 -> begin 
      match is_pair rem' with 
      | Pair (r'', rem'') when length rem'' = 0 -> 
        FullHouse (r'', r)
      | _ -> TwoPairs (r, r', return rem' 1) 
    end
  | _ -> Pair (r, return rem 3) 

let is_full_house deck =
  let cards = rev_sort deck in
  match is_pair cards with
  | Pair (r, rem) when length rem = 3 -> 
    FourOfAKind (r, return rem 1)
  | Pair (r, rem) when length rem = 4 -> begin
      match is_pair rem with 
      | Pair (r', rem') when length rem' = 0 -> 
        FourOfAKind (r', return cards 1)
      | Pair (r', rem') when length rem' = 1 || length rem' = 2 -> 
        FullHouse (r, r')
      |  _ -> ThreeOfAKind (r, return rem 2)
    end
  | Pair (r, rem) when length rem = 5 -> 
    house_helper r rem cards (is_pair rem) 
  | _ -> HighCard (return cards 5) 

let first deck = 
  match pick deck 0 with 
  | None -> failwith "impossible"
  | Some c -> c 

let sf_helper fl st_fl = function
  | true -> Flush (return fl 5)
  | false -> 
    let c = first st_fl in 
    if rank c = 14 then RoyalFlush 
    else StraightFlush (return fl 5)

let is_straight_flush deck = 
  let st = straight deck in
  let fl = flush deck in
  match (is_empty st, is_empty fl) with
  | false, false -> begin 
      let st_fl = straight_flush deck in
      sf_helper fl st_fl (is_empty st_fl)
    end
  | true, false -> Flush fl
  | false, true -> Straight st
  | true, true -> let cards = rev_sort deck in 
    HighCard (return cards 5)

let cmp_type t1 t2 =
  compare (hand_score t1) (hand_score t2)

let hand_value deck =  
  let t1 = is_straight_flush deck in 
  let t2 = is_full_house deck  in
  if cmp_type t1 t2 > 0 then t1 else t2

let get_cards = function
  | HighCard d -> d
  | Pair (r, d) -> d
  | TwoPairs (r1, r2, d) -> d
  | ThreeOfAKind (r, d) -> d
  | Straight d -> d
  | Flush d -> d
  | FourOfAKind (r, d) -> d 
  | StraightFlush d -> d
  | _ -> empty_deck()

let get_rank = function
  | Pair (r, d) -> r
  | TwoPairs (r1, r2, d) -> r1 
  | ThreeOfAKind (r, d) -> r
  | FullHouse (r1, r2) -> r1
  | FourOfAKind (r, d) -> r
  | _ -> 0

let get_sec_rank = function
  | TwoPairs (r1, r2, d) -> r2
  | FullHouse (r1, r2) -> r2
  | _ -> 0

let cmp_rank h1 h2 =
  compare (get_rank h1) (get_rank h2)

let cmp_sec_rank h1 h2 = 
  compare (get_sec_rank h1) (get_sec_rank h2)

let cmp_helper cmp1 cmp2 h1 h2 = 
  let c = cmp1 h1 h2 in 
  if c = 0 then cmp2 h1 h2 
  else c 

let cmp_hand c1 c2 = 
  let t1 = hand_value c1 in 
  let t2 = hand_value c2 in 
  let c = cmp_helper cmp_type cmp_rank t1 t2 in   
  if c = 0 then 
    cmp_high_cards (get_cards t1) (get_cards t2) 
  else c 

