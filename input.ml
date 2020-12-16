open ANSITerminal
open Baccarat

let input_prompt = "> "
let game_seln_msg = "Please enter the name of the game you want to play: \
                     Blackjack, Poker, Baccarat.\n"
let no_such_game_msg = "You entered an invalid game name. Please try again.\n"
let no_entry_msg = "Please enter a number.\n"
let bet_on_msg = "Please enter whom you wish to bet on. You can either enter \
                  'banker' or 'player'or 'tie'. \n"
let invalid_bet_on_msg = "You have entered an invalid name to bet on. Please \
                          enter either 'banker' or 'player' or 'tie' \n"
let currency_msg = "Please enter your unit of currency.\n"
let yes_or_no_reminder = "Please enter either 'y' or 'n'.\n"

(** List of names of supported games. *)
let games = ["blackjack"; "poker"; "baccarat"]

let rec get_gamemode () =
  print_endline game_seln_msg;
  print_string [] input_prompt;
  let name = read_line () |> String.trim |> String.lowercase_ascii in 
  if List.mem name games then name 
  else begin 
    print_endline no_such_game_msg; 
    get_gamemode () 
  end

let rec choose_num_geq_1_leq_n initial_prompt invalid_msg cap exists_limit = 
  print_endline initial_prompt;
  print_string [] input_prompt;
  let n = read_line () |> int_of_string_opt in
  match n with
  | Some num ->
    if exists_limit then
      if num > 0 && num <= cap then num else begin
        print_endline invalid_msg;
        choose_num_geq_1_leq_n initial_prompt invalid_msg cap exists_limit
      end else
    if num > 0 then num else begin
      print_endline invalid_msg;
      choose_num_geq_1_leq_n initial_prompt invalid_msg cap exists_limit
    end
  | None -> print_endline no_entry_msg;
    choose_num_geq_1_leq_n initial_prompt invalid_msg cap exists_limit

let rec choose_bet () =
  print_endline bet_on_msg;
  let n = read_line () |> String.trim |> String.lowercase_ascii in
  match n with
  | "banker" -> Banker
  | "player" -> Player 
  | "tie" -> Tie
  | _ -> print_endline invalid_bet_on_msg; choose_bet ()

let update_currency () =
  print_endline currency_msg;
  print_string [] input_prompt;
  read_line () 

let rec yes_or_no prompt = 
  prompt |> print_endline;
  print_string [] input_prompt;
  let response = read_line () |> String.trim |> String.lowercase_ascii in
  if response = "y" then true else if response = "n" then false
  else begin 
    print_endline yes_or_no_reminder;
    yes_or_no prompt
  end