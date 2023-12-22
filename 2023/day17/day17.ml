open Core

let lines_to_mat lines = 
  lines
  |> List.to_array
  |> Array.map ~f:String.to_array
  |> Array.map ~f:(Array.map ~f:(fun ch -> Int.of_string (Char.to_string ch)))

module Node = struct
  (*x, y, current_run, x_dir, y_dir *)
  type t = int * int * int * int * int

  [@@deriving sexp_of, of_sexp, hash, compare]
end

let compare_heat h_node_1 h_node_2 = 
  let h1, _, _, _, _, _ = h_node_1 in
  let h2, _, _, _, _, _ = h_node_2 in
  Int.compare h1 h2

let directions = [(-1, 0); (0, -1); (0, 1); (1, 0)]

let is_visited visited node =
  let x, y, r, dx, dy = node in
  List.range 1 (r + 1)
  |> List.map ~f:(fun run -> (x, y, run, dx, dy))
  |> List.filter ~f:(Hash_set.mem visited)
  |> List.length > 0

let find_path maze = 
  let height = Array.length maze in
  let width = Array.length maze.(0) in
  let goal_x, goal_y = (height - 1, width - 1) in
  let openSet = Pairing_heap.of_list [(0, 0, 0, 0, 0, 0)] ~cmp:compare_heat in
  let visited = Hash_set.create (module Node) in
  let result = ref (-1) in

  while not (Pairing_heap.is_empty openSet) do
    let current = Pairing_heap.pop_exn openSet in
    let cur_heat_l, x, y, run, dx, dy = current in

    if x = goal_x && y = goal_y then (
      result := cur_heat_l;
      Pairing_heap.clear openSet;
    ) 
    else if is_visited visited (x, y, run, dx, dy) then ()
    else (
      Hash_set.add visited (x, y, run, dx, dy);
      List.iter directions ~f:(fun (new_dx, new_dy) ->
        let straight = (dx = new_dx && dy = new_dy) in
        let new_x, new_y = x + new_dx, y + new_dy in

        if (new_dx = (-dx) && new_dy = (-dy)) 
          || (run = 3 && straight) 
          || new_x < 0 || new_y < 0
          || new_x = height || new_y = width then ()
        else
          let new_run = if straight then run + 1 else 1 in
          Pairing_heap.add openSet (
            cur_heat_l + maze.(new_x).(new_y),
            new_x, new_y, new_run, new_dx, new_dy
          )
      )
    )
  done;
  !result

let solve input =
  input
  |> lines_to_mat
  |> find_path

let find_path_2 maze = 
  let height = Array.length maze in
  let width = Array.length maze.(0) in
  let goal_x, goal_y = (height - 1, width - 1) in
  let openSet = Pairing_heap.of_list [(0, 0, 0, 0, 0, 1); (0, 0, 0, 0, 1, 0)] ~cmp:compare_heat in
  let visited = Hash_set.create (module Node) in
  let result = ref (-1) in

  while not (Pairing_heap.is_empty openSet) do
    let current = Pairing_heap.pop_exn openSet in
    let cur_heat_l, x, y, run, dx, dy = current in

    if x = goal_x && y = goal_y && run >= 4 then (
      result := cur_heat_l;
      Pairing_heap.clear openSet;
    )
    else if Hash_set.mem visited (x, y, run, dx, dy) then ()
    else (
      Hash_set.add visited (x, y, run, dx, dy);
      List.iter directions ~f:(fun (new_dx, new_dy) ->
        let straight = (dx = new_dx && dy = new_dy) in
        let new_x, new_y = x + new_dx, y + new_dy in

        if (new_dx = (-dx) && new_dy = (-dy)) 
          || (run = 10 && straight)
          || (run < 4 && not straight)
          || new_x < 0 || new_y < 0
          || new_x = height || new_y = width then ()
        else
          let new_run = if straight then run + 1 else 1 in
          Pairing_heap.add openSet (
            cur_heat_l + maze.(new_x).(new_y),
            new_x, new_y, new_run, new_dx, new_dy
          )
      )
    )
  done;
  !result

let solve2 input = 
  input
  |> lines_to_mat
  |> find_path_2

let () =
  In_channel.with_file "./2023/day17/input" ~f:(fun ch -> 
    let lines = In_channel.input_lines ch in
    Printf.printf "Day 17 - Answer 1: %d \n" (solve lines);
    Printf.printf "Day 17 - Answer 2: %d \n" (solve2 lines);
  )
