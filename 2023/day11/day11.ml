open Core

let lines_to_matrix input = 
  input
  |> Array.of_list
  |> Array.map ~f:String.to_array

let find_galaxies universe = 
  universe
  |> Array.mapi ~f:(fun i row -> 
    Array.filter_mapi row ~f:(fun j e -> 
      if Char.equal '#' e then Some (i, j) else None))
  |> Array.to_list
  |> Array.concat

let is_empty row = 
  Array.for_all row ~f:(Char.equal '.')

let is_empty_col matrix col = 
  Array.for_all matrix ~f:(fun row -> Char.equal row.(col) '.')

let rows_expanded universe from to_ = 
  if from = to_ then 0
  else
    Array.slice universe from to_
    |> Array.count ~f:is_empty

let cols_expanded universe from to_ = 
  List.range from to_
  |> List.count ~f:(is_empty_col universe)

let dist universe mult (x1, y1) (x2, y2) = 
  abs (x1 - x2) + abs (y1 - y2) 
  + (rows_expanded universe (min x1 x2) (max x1 x2)) * mult
  + (cols_expanded universe (min y1 y2) (max y1 y2)) * mult

let pairs arr = 
  let result = ref([]) in
  for i = 0 to (Array.length arr - 2) do
    for j = i + 1 to (Array.length arr - 1) do
      result := (arr.(i), arr.(j)) :: !result
    done
  done;
  !result

let solve mult input = 
  let universe = input
    |> lines_to_matrix in
  let galaxies = find_galaxies universe in
  galaxies
  |> pairs
  |> List.map ~f:(fun (g1, g2) -> dist universe mult g1 g2)
  |> List.reduce_exn ~f:(+)

let () =
  In_channel.with_file "./2023/day11/input" ~f: (fun ch -> 
    let lines = In_channel.input_lines ch in
    Printf.printf "Day 11 - Answer 1: %d \n" (solve 1 lines);
    Printf.printf "Day 11 - Answer 2: %d \n" (solve 999999 lines);
  )
