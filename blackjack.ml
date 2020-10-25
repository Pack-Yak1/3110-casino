type outcome = 
  | DealerBust
  | Match of int

let initial_cards = 2

let win_check outcome player_score : bool =
  match outcome with
  | DealerBust -> true
  | Match x -> begin
      if player_score = -1 then true
      else if player_score > x && player_score <= 21 then true
      else false
    end