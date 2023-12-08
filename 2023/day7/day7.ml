open Core

let list_add_count assoc ch = 
  match List.Assoc.find ~equal:Char.equal assoc ch with
  | Some c -> List.Assoc.add ~equal:Char.equal assoc ch (c + 1)
  | None -> List.Assoc.add ~equal:Char.equal assoc ch 1

let get_hand_and_bid line = 
  let split = String.split line ~on:' ' in
  (List.hd_exn split, Int.of_string(List.last_exn split))

let count_hand hand = 
  hand
  |> String.to_list
  |> List.fold_left ~init:[] ~f:(fun acc ch -> list_add_count acc ch)

let jokers_count hand = 
  let jokers = List.Assoc.find hand 'J' ~equal:Char.equal in
  Option.value jokers ~default:0

let is_five ?(jokers = 0) hand_count = 
  List.length hand_count = 1 || (List.length hand_count = 2 && jokers > 0)

let not_joker ch = 
  not (Char.equal ch 'J')

let is_four ?(jokers = 0) hand_count = 
  List.exists hand_count ~f:(fun (l, c) -> (c + jokers = 4) && not_joker l)

let is_two_pair hand_count = 
  let count_pairs = List.count hand_count ~f:(fun (_, c) -> c = 2) in
  count_pairs = 2

let is_full_house ?(jokers = 0) hand_count = 
  let has_two = List.exists hand_count ~f:(fun (l, c) -> (c = 2) && not_joker l) in
  let has_two_j = List.exists hand_count ~f:(fun (l, c) -> (c + jokers = 2) && not_joker l) in
  let has_three = List.exists hand_count ~f:(fun (l, c) -> (c = 3) && not_joker l) in
  let has_three_j = List.exists hand_count ~f:(fun (l, c) -> (c + jokers = 3) && not_joker l) in
  (has_two && has_three) || (has_two_j && has_three) || (has_two && has_three_j && (is_two_pair hand_count))

let is_three ?(jokers = 0) hand_count = 
  List.exists hand_count ~f:(fun (_, c) -> c + jokers = 3)

let is_pair ?(jokers = 0) hand_count = 
  List.exists hand_count ~f:(fun (_, c) -> c + jokers = 2) 

let score hand = 
  let count = count_hand hand in
  if is_five count then 6
  else if is_four count then 5
  else if is_full_house count then 4
  else if is_three count then 3
  else if is_two_pair count then 2
  else if is_pair count then 1
  else 0

let score_j hand = 
  let count = count_hand hand in
  let j_count = jokers_count count in
  if is_five ~jokers:j_count count then 6
  else if is_four ~jokers:j_count count then 5
  else if is_full_house ~jokers:j_count count then 4
  else if is_three ~jokers:j_count count then 3
  else if is_two_pair count then 2
  else if is_pair ~jokers:j_count count then 1
  else 0

let labels = [('A', 13); ('K', 12); ('Q', 11); ('T', 9); ('9', 8); ('8', 7);
  ('7', 6); ('6', 5); ('5', 4); ('4', 3); ('3', 2); ('2', 1); ('J', 0)]

let get_label_score label = 
  List.Assoc.find_exn ~equal:Char.equal labels label

let rec compare_hands_by_label hand1 hand2 =
  match hand1, hand2 with 
  | [] , [] -> 0
  | h1 :: t1, h2 :: t2 when Char.equal h1 h2 -> compare_hands_by_label t1 t2
  | h1 :: _, h2 :: _ -> Int.compare (get_label_score h1) (get_label_score h2)
  | _ -> 0

let compare_hands hand1 hand2 = 
  let hands_comp = Int.compare (score hand1) (score hand2) in
  if hands_comp = 0 
  then compare_hands_by_label (String.to_list hand1) (String.to_list hand2) 
  else hands_comp

let total_winnings input = 
  input
  |> List.map ~f:get_hand_and_bid
  |> List.sort ~compare:(fun (h1, _) (h2, _) -> compare_hands h1 h2)
  |> List.mapi ~f:(fun i (_, b) -> (i + 1), b)
  |> List.map ~f:(fun (r, b) -> r * b)
  |> List.reduce_exn ~f:(+)

let compare_hands_j hand1 hand2 = 
  let hands_comp = Int.compare (score_j hand1) (score_j hand2) in
  if hands_comp = 0 
  then compare_hands_by_label (String.to_list hand1) (String.to_list hand2) 
  else hands_comp

let total_winnings_j input = 
  input
  |> List.map ~f:get_hand_and_bid
  |> List.sort ~compare:(fun (h1, _) (h2, _) -> compare_hands_j h1 h2)
  |> List.mapi ~f:(fun i (_, b) -> (i + 1), b)
  |> List.map ~f:(fun (r, b) -> r * b)
  |> List.reduce_exn ~f:(+)


let main () =
  In_channel.with_file "./2023/day7/input.txt" ~f: (fun ch -> 
    let lines = In_channel.input_lines ch in
    Printf.printf "Day 7 - Answer 1: %d \n" (total_winnings lines);
    Printf.printf "Day 7 - Answer 2: %d \n" (total_winnings_j lines);
  );;


main ();;