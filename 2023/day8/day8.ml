open Core

let split_strip ~on str =
  str 
  |> String.split ~on:on
  |> List.map ~f:(String.strip ~drop:(fun ch -> Char.equal ch ' ' || Char.equal ch '(' || Char.equal ch ')'))

let list_to_tuple list =
  match list with
  | a :: b :: [] -> (a, b)
  | _ -> raise (Invalid_argument "not a 2 elem list")

let find map k = 
  Map.find_exn map k

let navigate map nav key step stop_cond = 
  let rec recur n key step = 
    if stop_cond key then step
    else match n with
      | 'L' :: tl -> recur tl (Tuple2.get1 (find map key)) (step + 1)
      | 'R' :: tl -> recur tl (Tuple2.get2 (find map key)) (step + 1)
      | [] -> recur nav key step
      | _ -> 0 
  in
  recur nav key step

let count_steps input = 
  let nav = String.to_list (List.hd_exn input) in
  let map = List.drop input 2
  |> List.map ~f:(split_strip ~on:'=')
  |> List.map ~f:list_to_tuple
  |> List.map ~f:(fun (key, pair) -> (key, list_to_tuple (split_strip ~on:',' pair))) 
  |> Map.of_alist_exn (module String) in
  navigate map nav "AAA" 0 (String.equal "ZZZ")

let ends_with ~l str = 
  Char.equal l (Array.last (String.to_array str))

let starting_keys map = 
  map
  |> Map.keys
  |> List.filter ~f:(ends_with ~l:'A')

let count_steps_ghost input = 
  let nav = String.to_list (List.hd_exn input) in
  let map = List.drop input 2
    |> List.map ~f:(split_strip ~on:'=')
    |> List.map ~f:list_to_tuple
    |> List.map ~f:(fun (key, pair) -> (key, list_to_tuple (split_strip ~on:',' pair))) 
    |> Map.of_alist_exn (module String) in
  let start_keys = starting_keys map in
  List.map start_keys ~f:(fun k -> navigate map nav k 0 (ends_with ~l:'Z'))
    |> List.reduce_exn ~f:(fun a b -> let open Z in to_int (lcm (of_int a) (of_int b)))

let () =
  In_channel.with_file "./2023/day8/input.txt" ~f: (fun ch -> 
    let lines = In_channel.input_lines ch in
    Printf.printf "Day 8 - Answer 1: %d \n" (count_steps lines);
    Printf.printf "Day 8 - Answer 2: %d \n" (count_steps_ghost lines);
  )