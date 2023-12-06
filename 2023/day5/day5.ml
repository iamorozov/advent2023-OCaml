open Core

let flatten list_of_lists = 
  list_of_lists
  |> List.fold_left ~init:[] ~f:List.append

let tuple3_of_list list = 
  match list with
  | a :: b :: c :: [] -> (a, b, c)
  | _ -> (0, 0, 0)

let tuple2_of_list list = 
  match list with
  | a :: b :: [] -> (a, b)
  | _ -> (0, 0)

let lookup s map = 
  let range = List.find map ~f:(fun (_, source, offset) -> source <= s && (source + offset) > s) in
  match range with
  | Some (dest, source, _) -> dest + (s - source)
  | None -> s

let rec lookup_through maps v = 
  match maps with
  | [] -> v
  | x :: xs -> lookup_through xs (lookup v x)

let create_lookup_map lines = 
  lines
  |> List.map ~f:(String.split ~on:' ')
  |> List.map ~f:(List.map ~f:Int.of_string)
  |> List.map ~f:tuple3_of_list

let extract_seeds seeds = 
  seeds
  |> String.split ~on:':'
  |> List.last_exn
  |> String.strip
  |> String.split ~on:' '
  |> List.map ~f:Int.of_string

let extract_seed_ranges seeds = 
  seeds
  |> extract_seeds
  |> List.chunks_of ~length:2
  |> List.map ~f:tuple2_of_list

let closest_seed_location input = 
  let seeds = List.hd_exn input |> extract_seeds in
  let maps = input
    |> List.tl_exn
    |> List.group ~break:(fun _ b -> String.equal b "")
    |> List.map ~f:(fun map -> List.drop map 2)
    |> List.map ~f:create_lookup_map in
  seeds
    |> List.map ~f:(lookup_through maps)
    |> List.min_elt ~compare:Int.compare
    |> Option.value_exn

let map_range map range = 
  let (from, len) = range in
  let (dest, source, len_m) = map in
  let offset = dest - source in
  let to_ = from + len in
  let to_m = source + len_m in
  if from >= source && to_ <= to_m then ([(from + offset, len)], [])
  else if from < source && to_ > source && to_ <= to_m then ([(dest, len - source + from)], [(from, source - from)])
  else if from < to_m && to_ > to_m && from >= source then ([(from + offset, to_m - from)], [(to_m, to_ - to_m)])
  else if from < source && to_ > to_m then ([(dest, len_m)], [(from, source - from); (to_m, to_ - to_m)])
  else ([], [(from, len)])

let map_ranges_through ranges mappings: (int * int) list = 
  let (l1, l2) = List.fold_left mappings ~init:([], ranges) ~f:(fun acc map ->
    let (processed, unprocessed) = acc in
    match unprocessed with
    | [] -> (processed, unprocessed)
    | unpr -> let (newP, newU) = List.map unpr ~f:(map_range map)
      |> List.fold_left ~init:([], []) ~f:(fun (pa, ua) (p, u) -> (List.append pa p, List.append ua u)) in
      (List.append processed newP, newU)
  ) in
  List.append l1 l2

let rec map_ranges_through_maps ranges maps = 
  match maps with
  | [] -> ranges
  | x :: xs -> map_ranges_through_maps (map_ranges_through ranges x) xs
  
let closest_seed_location_expanded input = 
  let seeds = List.hd_exn input |> extract_seed_ranges in
  let maps = input
    |> List.tl_exn
    |> List.group ~break:(fun _ b -> String.equal b "")
    |> List.map ~f:(fun map -> List.drop map 2)
    |> List.map ~f:create_lookup_map in
  seeds
  |> List.map ~f:(fun seed -> map_ranges_through_maps [seed] maps |> List.map ~f:Tuple2.get1)
  |> flatten
  |> List.min_elt ~compare:Int.compare
  |> Option.value_exn

let main () =
  In_channel.with_file "./2023/day5/input.txt" ~f: (fun ch -> 
    let lines = In_channel.input_lines ch in
    Printf.printf "Day 5 - Answer 1: %d \n" (closest_seed_location lines);
    Printf.printf "Day 5 - Answer 2: %d \n" (closest_seed_location_expanded lines);
  );;


main ();;