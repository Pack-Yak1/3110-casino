type outcome = 
  | DealerBust
  | Match of int

let initial_cards = 2

let win_check player_score outcome : bool = 
  match outcome with
  | DealerBust -> true
  | Match x -> begin
      if x = -1 then true
      else if player_score > x &&  player_score <= 21 then true
      else false
    end