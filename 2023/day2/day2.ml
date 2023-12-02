open Core

let red_max = 12
let green_max = 13
let blue_max = 14

let is_valid cubes = 
  match String.split cubes ~on:' ' with
  | num :: "red" :: [] -> Int.of_string num <= red_max
  | num :: "green" :: [] -> Int.of_string num <= green_max
  | num :: "blue" :: [] -> Int.of_string num <= blue_max
  | _ -> false

let extract_cubes round = 
  String.split ~on:',' round 
  |> List.map ~f:String.strip

let is_valid_round round =
  extract_cubes round
  |> List.for_all ~f:is_valid

let extract_rounds game = 
  String.split ~on:';' game 
  |> List.map ~f:String.strip;;

let is_valid_game game = 
  extract_rounds game
  |> List.for_all ~f:is_valid_round

let extract_game game = 
  let split = String.split ~on:':' game in
  (
    Int.of_string (List.last_exn (String.split ~on:' ' (List.hd_exn split))), 
    String.strip (List.last_exn split)
  )

let count_valid_games input =
  List.fold_left input ~init:0 ~f: (fun acc line ->
    let (game_num, game) = extract_game line in
    if is_valid_game game then acc + game_num else acc
  )

let find_max_cubes game_str = 
  let (_, game) = extract_game game_str in
    extract_rounds game
    |> List.map ~f:extract_cubes
    |> List.fold_left ~init:[] ~f:List.append
    |> List.map ~f: (fun s -> String.split ~on:' ' s)
    |> List.map ~f: (fun l -> (Int.of_string (List.hd_exn l), List.last_exn l))
    |> List.fold_left ~init:(0, 0, 0) ~f: (fun (r, g, b) (num, color) -> 
      match color with
      | "red" -> if num > r then (num, g, b) else (r, g, b)
      | "green" -> if num > g then (r, num, b) else (r, g, b)
      | "blue" -> if num > b then (r, g, num) else (r, g, b)
      | _ -> (r, g, b))

let count_power_sum input =
  List.fold_left input ~init:0 ~f: (fun acc line ->
    let (r, g, b) = find_max_cubes line in
    acc + (r * g * b)
  )

let print_results result1 result2 = 
  Printf.printf "Day 2 - Answer 1: %d \n" result1;
  Printf.printf "Day 2 - Answer 2: %d \n" result2

let main () =
  In_channel.with_file "./2023/day2/input.txt" ~f: (fun ch -> 
    let lines = In_channel.input_lines ch in
    print_results (count_valid_games lines) (count_power_sum lines)
  );;


main ();;