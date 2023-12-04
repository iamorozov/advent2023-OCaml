open Core

let extract_points card =
  card
  |> String.split ~on:':'
  |> List.last_exn
  |> String.strip
  |> String.split ~on:'|'
  |> List.map ~f:String.strip
  |> List.map ~f:(String.split ~on:' ')
  |> List.map ~f:(List.filter ~f:(fun e -> not (String.equal e "")))
  |> List.map ~f:(Set.of_list (module String))
  |> List.reduce_exn ~f:Set.inter
  |> Set.to_list
  |> List.length

let extract_game card =
  card
  |> String.split ~on:':'
  |> List.hd_exn
  |> String.split ~on:' '
  |> List.last_exn
  |> Int.of_string

let card_score card = 
  let matches = extract_points card in
  if matches > 0 then Int.pow 2 (matches - 1) else 0

let cards_sum lines = 
  lines
  |> List.map ~f:card_score
  |> List.reduce_exn ~f:(+)

let list_add assoc key value = 
  match List.Assoc.find ~equal:Int.equal assoc key with
  | Some v -> List.Assoc.add ~equal:Int.equal assoc key (value + v)
  | None -> List.Assoc.add ~equal:Int.equal assoc key value

let list_get_or_zero assoc key = 
  match List.Assoc.find ~equal:Int.equal assoc key with
  | Some v -> v
  | None -> 0

let rec update_copies copies from to_ num = 
  if from > to_ 
    then copies 
    else update_copies (list_add copies from num) (from + 1) to_ num

let copies_sum lines = 
  let copies = List.fold_left lines ~init:[] ~f:(fun acc line ->
    let points = extract_points line in
    let card_num = extract_game line in
    let card_copies = list_get_or_zero acc card_num in
    let copies_won = card_copies + 1 in
    update_copies acc (card_num + 1) (card_num + points) copies_won
  ) in
  let total_copies = List.fold_left copies ~init:0 ~f:(fun acc (_, v) -> acc + v) in
  total_copies + List.length lines
  

let main () =
  In_channel.with_file "./2023/day4/input.txt" ~f: (fun ch -> 
    let lines = In_channel.input_lines ch in
    Printf.printf "Day 4 - Answer 1: %d \n" (cards_sum lines);
    Printf.printf "Day 4 - Answer 2: %d \n" (copies_sum lines)
  );;


main ();;