open Core

let char_comp c1 c2 = Char.equal c1 c2
let ( =^ ) = char_comp

let lines_to_mat lines = 
  lines
  |> List.to_array
  |> Array.map ~f:String.to_array

type direction =
  | UP
  | DOWN
  | LEFT
  | RIGHT
[@@deriving equal]

let move i j dir cell = 
  match dir with
  | UP -> (
    match cell with
    | '.' | '|' -> (i - 1, j, UP)
    | '/' -> (i, j + 1, RIGHT)
    | '\\' -> (i, j - 1, LEFT)
    | _ -> failwith "impossibru"
  )
  | DOWN -> (
    match cell with
    | '.' | '|' -> (i + 1, j, DOWN)
    | '/' -> (i, j - 1, LEFT)
    | '\\' -> (i, j + 1, RIGHT)
    | _ -> failwith "impossibru"
  )
  | LEFT -> (
    match cell with
    | '.' | '-' -> (i, j - 1, LEFT)
    | '/' -> (i + 1, j, DOWN)
    | '\\' -> (i - 1, j, UP)
    | _ -> failwith "impossibru"
  )
  | RIGHT -> (
    match cell with
    | '.' | '-' -> (i, j + 1, RIGHT)
    | '/' -> (i - 1, j, UP)
    | '\\' -> (i + 1, j, DOWN)
    | _ -> failwith "impossibru"
  )

let split_move i j dir cell = 
  match dir, cell with
  | LEFT, '|' | RIGHT, '|' -> ((i - 1, j, UP), (i + 1, j, DOWN))
  | UP, '-' | DOWN, '-' -> ((i, j - 1, LEFT), (i, j + 1, RIGHT))
  | _ -> failwith "impossibru"

let start maze visited height width start_i start_j start_dir = 
  let should_split dir cell =
    (cell =^ '|') && (equal_direction dir LEFT || equal_direction dir RIGHT)
    || (cell =^ '-') && (equal_direction dir UP || equal_direction dir DOWN)
  in

  let out_of_bounds i j = i < 0 || i >= height || j < 0 || j >= width in

  let is_visited i j dir = 
    let (cell, dirs) = visited.(i).(j) in
      cell =^ '#' && List.mem dirs ~equal:(equal_direction) dir
  in

  let rec step i j dir =
    if out_of_bounds i j || is_visited i j dir then ()
    else (
      let (_, list) = visited.(i).(j) in 
        visited.(i).(j) <- ('#', dir :: list);

      if should_split dir maze.(i).(j) then (
        let (i_new_1, j_new_1, dir_new_1), (i_new_2, j_new_2, dir_new_2) = 
          split_move i j dir maze.(i).(j) in
        step i_new_1 j_new_1 dir_new_1;
        step i_new_2 j_new_2 dir_new_2;
      )
      else
        let (new_i, new_j, new_dir) = move i j dir maze.(i).(j) in
          step new_i new_j new_dir
    ) in
  step start_i start_j start_dir

let find_energized maze i j dir = 
  let height = Array.length maze in
  let width = Array.length maze.(0) in
  let visited = Array.make_matrix ~dimx:height ~dimy:width ('.', []) in
  let () = start maze visited height width i j dir in
  visited 
    |> Array.map ~f:(fun row -> Array.count row ~f:(fun (ch, _) -> ch =^ '#')) 
    |> Array.reduce_exn ~f:(+)

let solve input =
  let maze = input |> lines_to_mat in
  find_energized maze 0 0 RIGHT

let find_max maze = 
  let max_en = ref 0 in
  let height = Array.length maze in
  let width = Array.length maze.(0) in
  for i = 0 to height - 1 do max_en := max !max_en (find_energized maze i 0 RIGHT) done;
  for i = 0 to height - 1 do max_en := max !max_en (find_energized maze i (width - 1) LEFT) done;
  for j = 0 to width - 1 do max_en := max !max_en (find_energized maze 0 j DOWN) done;
  for j = 0 to width - 1 do max_en := max !max_en (find_energized maze (height - 1) j UP) done;
  !max_en

let solve2 input = 
  let maze = input |> lines_to_mat in
  find_max maze

let () =
  In_channel.with_file "./2023/day16/input" ~f:(fun ch -> 
    let lines = In_channel.input_lines ch in
    Printf.printf "Day 16 - Answer 1: %d \n" (solve lines);
    Printf.printf "Day 16 - Answer 2: %d \n" (solve2 lines);
  )
