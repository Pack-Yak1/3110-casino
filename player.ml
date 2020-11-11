open Deck

type t = {
  name : string;
  mutable hand : Deck.t;
  mutable bet : int;
  mutable money : int;
  mutable style : ANSITerminal.style list
}

let default_allowance = 100

let default_player = {
  name = "";
  hand = empty_deck ();
  bet = 0;
  money = default_allowance;
  style = []
}

let dealer = {
  name = "dealer";
  hand = empty_deck ();
  bet = 0;
  money = max_int;
  style = []
}