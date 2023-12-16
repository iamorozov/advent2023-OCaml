open Core

let char_comp c1 c2 = Char.equal c1 c2
let ( =^ ) = char_comp

let lines_to_mat lines = 
  lines
  |> List.to_array
  |> Array.map ~f:String.to_array

let calculate_weights dish = 
  let height = Array.length dish in
  let width = Array.length dish.(0) in
  let weights = Array.make_matrix ~dimx:height ~dimy:width 0 in
  let result = ref 0 in
  for i = 0 to height - 1 do
    for j = 0 to width - 1 do
      let load = height - i in
      let upper_load = if i > 0 then weights.(i - 1).(j) else 0 in
      let cur_load = max load upper_load in
      let cell = dish.(i).(j) in

      if Char.equal cell '.' then
        weights.(i).(j) <- cur_load
      else if Char.equal cell '#' then
        weights.(i).(j) <- 0
      else (
        result := !result + cur_load;
        weights.(i).(j) <- cur_load - 1;
      )
    done
  done;
  !result

let solve input = 
  input
  |> lines_to_mat
  |> calculate_weights

let is_stone ch = ch =^ '#' || ch =^ 'O'

let tilt_row row ~reverse = 
  let len = Array.length row in
  let rev_row = if reverse then Array.rev row else Array.copy row in
  for i = 0 to len - 1 do
    if rev_row.(i) =^ 'O' then (
      let k = ref (i - 1) in
      while !k >= 0 && not (is_stone rev_row.(!k)) do
        rev_row.(!k + 1) <- '.';
        rev_row.(!k) <- 'O';
        k := !k - 1;
      done
    )
  done;
  if reverse then Array.rev rev_row else rev_row

let tilt dish ~reverse = 
  dish |> Array.map ~f:(tilt_row ~reverse:reverse)

let cycle dish = 
  dish 
  |> Array.transpose_exn |> tilt ~reverse:false
  |> Array.transpose_exn |> tilt ~reverse:false
  |> Array.transpose_exn |> tilt ~reverse:true
  |> Array.transpose_exn |> tilt ~reverse:true

let north_load dish =
  let height = Array.length dish in
  dish
  |> Array.mapi ~f:(fun i row -> (height - i) * Array.count row ~f:((=^) 'O'))
  |> Array.reduce_exn ~f:(+)

module Dish = struct
  module T = struct
    type t = char array array [@@deriving compare, sexp_of]
  end
  include T
  include Comparable.Make_plain(T)
end

let solve2 input = 
  let dish = input |> lines_to_mat in
  let spin = ref dish in
  let hist = ref (Map.empty (module Dish)) in
  for i = 0 to 154 do (*this was calculated after finding a cycle*)
    spin := cycle !spin;
    match Map.find !hist !spin with
    | None -> hist := Map.add_exn !hist ~key:!spin ~data:i
    | Some n -> Printf.printf "Cycle: n = %d i = %d load = %d \n" n i (north_load !spin)
  done;
  north_load !spin

let () =
  In_channel.with_file "./2023/day14/input" ~f:(fun ch -> 
    let lines = In_channel.input_lines ch in
    Printf.printf "Day 14 - Answer 1: %d \n" (solve lines);
    Printf.printf "Day 14 - Answer 2: %d \n" (solve2 lines);
  )
