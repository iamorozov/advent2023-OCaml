open Core

(* Code that was the first approach but turned out to be good enough only for visualization *)

let strip_brackets = String.strip ~drop:(fun ch -> Char.equal '(' ch || Char.equal ')' ch)

let parse_line line = 
  let [dir; num; color] = line |> String.split ~on:' ' in
  (dir, Int.of_string num, strip_brackets color)

let merge line =
  let rec go l cur res = 
    match l, cur with
    | [], c -> res @ [c]
    | x :: xs, c -> 
      let x_start, x_len = x in
      let c_start, c_len = c in
      if c_start + c_len = x_start 
        then go xs (c_start, c_len + x_len) res
        else go xs x (res @ [cur]) in
  go (List.tl_exn line) (List.hd_exn line) []

let update_level levels level range = 
  Hashtbl.update levels level ~f:(fun opt_level ->
    match opt_level with
    | None -> [range]
    | Some l ->
        let new_start, _ = range in
        let less, more = List.partition_tf l ~f:(fun (s, _) -> s < new_start) in
        less @ [range] @ more
   )

let dig terrain plan = 
  let x = ref 0 in
  let y = ref 0 in
  plan
  |> List.iter ~f:(fun (dir, num, _) -> 
      match dir with
      | "R" -> update_level terrain !x (!y + 1, num); y := !y + num;
      | "L" -> update_level terrain !x (!y - num, num); y := !y - num;
      | "D" -> for i = 1 to num do
          update_level terrain (!x + 1) (!y, 1); x := !x + 1;
        done
      | "U" -> for i = 1 to num do
          update_level terrain (!x - 1) (!y, 1); x := !x - 1;
        done
    )

let area_between (s1, _) (s2, l2) = s2 + l2 - s1

let area_line line = 
  line
  |> List.groupi ~break:(fun i _ _ -> i mod 2 = 0)
  |> List.map ~f:(fun ranges ->
      match ranges with
      | [r1; r2] -> area_between r1 r2
      | [(s, l)] -> l
      | _ -> 0
    )
  |> List.reduce_exn ~f:(+)

let area_terrain t = 
  t
  |> Hashtbl.to_alist
  |> List.map ~f:(fun (_, ranges) -> merge ranges)
  |> List.map ~f:area_line
  |> List.reduce_exn ~f:(+)

let get_min_range ranges = 
  let s, _ = List.hd_exn ranges in
  s

let visualize terrain = 
  let sorted = terrain
    |> Hashtbl.to_alist
    |> List.map ~f:(fun (i, ranges) -> (i, merge ranges))
    |> List.sort ~compare:(fun (i1, _) (i2, _) -> Int.compare i1 i2) in
  let min_x = sorted |> List.hd_exn |> Tuple2.get1 in
  let min_y = sorted 
    |> List.map ~f:(fun (_, r) -> get_min_range r) 
    |> List.min_elt ~compare:Int.compare 
    |> Option.value_exn in
  let offset = abs min_y in
  sorted
    |> List.map ~f:(fun (_, ranges) -> 
        let res = Array.create ~len:500 '.' in
        ranges
        |> List.iter ~f:(fun (s, l) -> for i = s + offset to s + offset + l - 1 do res.(i) <- '#' done);
        res
        |> Array.to_list 
        |> String.of_char_list
      )
    |> Out_channel.write_lines "2023/day18/out"

let solve input =
  let dig_plan = input |> List.map ~f:parse_line in
  let terrain = Hashtbl.create (module Int) in
  dig terrain dig_plan;
  area_terrain terrain

(* Solution with shoelace formula *)

let collect_coordinates dig_plan = 
  let rec go plan x y result = 
    match plan with
    | [] -> result
    | (dir, num) :: tail -> 
      match dir with
      | "R" -> go tail x (y + num) ((x, y) :: result)
      | "L" -> go tail x (y - num) ((x, y) :: result)
      | "D" -> go tail (x + num) y ((x, y) :: result)
      | "U" -> go tail (x - num) y ((x, y) :: result)
  in
  go dig_plan 0 0 []

let det (x1, y1) (x2, y2) = x1 * y2 - y1 * x2

let inside_area coords = 
  let rec go tail result =
    match tail with
    | [] -> failwith "impossible"
    | x :: [] -> result + det x (List.hd_exn coords)
    | x :: y :: xs -> go (y :: xs) (result + det x y)
  in
  abs (go coords 0) / 2

let dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2) 

let perimeter coords = 
  let rec go tail result = 
    match tail with
    | [] -> failwith "impossible"
    | x :: [] -> result + dist x (List.hd_exn coords)
    | x :: y :: xs -> go (y :: xs) (result + dist x y)
  in
  go coords 0

let solve_shoelace input =
  let dig_plan = input 
    |> List.map ~f:parse_line
    |> List.map ~f:(fun (dir, num, _) -> (dir, num))
  in
  let coords = collect_coordinates dig_plan in
  inside_area coords + (perimeter coords) / 2 + 1

let convert_to_plan hex = 
  let dir = String.suffix hex 1 in
  let num = String.drop_suffix hex 1 
    |> String.chop_prefix_exn ~prefix:"#"
    |> (^) "0x"
    |> Int.of_string 
  in
  match dir with
  | "0" -> ("R", num)
  | "1" -> ("D", num)
  | "2" -> ("L", num)
  | "3" -> ("U", num)

let solve2 input = 
  let dig_plan = input 
    |> List.map ~f:parse_line
    |> List.map ~f:(fun (_, _, hex) -> convert_to_plan hex) in
  let coords = collect_coordinates dig_plan in
  inside_area coords + (perimeter coords) / 2 + 1

let () =
  In_channel.with_file "./2023/day18/input" ~f:(fun ch -> 
    let lines = In_channel.input_lines ch in
    Printf.printf "Answer 1: %d \n" (solve_shoelace lines);
    Printf.printf "Answer 2: %d \n" (solve2 lines);
  )
