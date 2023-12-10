open Core

module IntTuple = struct
  type t = int * int

  let compare (x0, y0) (x1, y1) =
    match Int.compare x0 x1 with
      0 -> Int.compare y0 y1
    | c -> c

  let t_of_sexp tuple = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp tuple
  let sexp_of_t tuple = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t tuple
end

module IntTupleSet = Set.Make(IntTuple)

let lines_to_matrix input = 
  input
  |> Array.of_list
  |> Array.map ~f:String.to_array

let start = 'S'

let find_start maze = 
  Array.find_mapi maze ~f:(fun i line -> 
    Array.findi line ~f:(fun _ elem -> Char.equal elem start)
      |> Option.map ~f:(fun (j, _) -> (i, j))
  ) |> Option.value_exn

let node_equal n1 n2 = 
  Tuple2.equal ~eq1:Int.equal ~eq2:Int.equal n1 n2

let loop maze start = 
  let rec go cur next visited len = 
    if node_equal start next then (len, (Set.add visited cur))
    else if Set.mem visited cur then (0, visited)
    else
      let i, j = cur in
      let i_next, j_next = next in
      let step = maze.(i_next).(j_next) in
      let new_ = 
        if Char.equal step '|' then
          if i < i_next then (i_next + 1, j) else (i_next - 1, j)
        else if Char.equal step '-' then
          if j < j_next then (i, j_next + 1) else (i, j_next - 1)
        else if Char.equal step 'L' then
          if i < i_next then (i_next, j_next + 1) else (i_next - 1, j_next)
        else if Char.equal step 'J' then
          if i < i_next then (i_next, j_next - 1) else (i_next - 1, j_next)
        else if Char.equal step '7' then
          if i > i_next then (i_next, j_next - 1) else (i_next + 1, j_next)
        else if Char.equal step 'F' then
          if i > i_next then (i_next, j_next + 1) else (i_next + 1, j_next)
        else failwith "impossible" in
       go next new_ (Set.add visited cur) (len + 1) in
  let (starti, startj) = start in
  let l1 = go start (starti + 1, startj) (IntTupleSet.empty) 1 in
  List.max_elt [l1] ~compare:(fun (a, _) (b, _) -> Int.compare a b)
  |> Option.value_exn

let solve1 input = 
  let maze = input
  |> lines_to_matrix in
  let start = find_start maze in
  let l, _ = loop maze start in
  l / 2

let line_area line =
  let walls = Set.of_list (module Char) ['|'; 'L'; 'J'] in
  let is_wall ch = 
    Set.mem walls ch
  in
  let need_count = ref false in
  let count = ref 0 in
  for i = 0 to (Array.length line - 1) do
    if is_wall line.(i) then need_count := not !need_count
    else if Char.equal line.(i) '.' && !need_count then count := !count + 1
    else ()
  done;
  !count

let replace maze loop = 
  maze
  |> Array.mapi ~f:(fun i line -> 
    line
    |> Array.mapi ~f:(fun j tile -> if Set.mem loop (i, j) then tile else '.'))

let solve2 input =
  let maze = input
    |> lines_to_matrix in
  let start = find_start maze in
  let _, visited = loop maze start in
  let replaced = replace maze visited in
  replaced
  |> Array.map ~f:line_area
  |> Array.reduce_exn ~f:(+)

let () =
  In_channel.with_file "./2023/day10/input" ~f: (fun ch -> 
    let lines = In_channel.input_lines ch in
    Printf.printf "Day 10 - Answer 1: %d \n" (solve1 lines);
    Printf.printf "Day 10 - Answer 2: %d \n" (solve2 lines);
  )
