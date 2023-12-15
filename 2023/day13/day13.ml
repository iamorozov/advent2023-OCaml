open Core

let group_patterns lines = 
  lines
  |> List.group ~break:(fun _ b -> String.equal "" b)
  |> List.map ~f:(fun p -> List.filter p ~f:(fun s -> not (String.equal "" s)))

let find_mirror_row rows = 
  rows
  |> Array.findi ~f:(fun n _ -> 
    let i = ref n in
    let j = ref (n + 1) in
    let len = Array.length rows in
    let is_mirror = ref false in
    while !i >= 0 && !j < len do
      if String.equal rows.(!i) rows.(!j) then (
        is_mirror := true;
        i := !i - 1;
        j := !j + 1;
      )
      else (
        is_mirror := false;
        i := -1;
      )
    done;
    !is_mirror
  )
  |> Option.map ~f:(fun (i, _) -> i + 1)
  |> Option.value ~default:0

let transpose_lines lines =
  lines
  |> Array.map ~f:String.to_array
  |> Array.transpose_exn
  |> Array.map ~f:(fun r -> r |> Array.to_list |> String.of_char_list)

let solve input = 
  input
  |> group_patterns
  |> List.map ~f:(fun pattern ->
      let rows = List.to_array pattern in
      let rows_mirror = find_mirror_row rows in
      let cols_mirror = if rows_mirror = 0 then
        let cols = transpose_lines rows in
        find_mirror_row cols
      else 0 in
      rows_mirror * 100 + cols_mirror
    )
  |> List.reduce_exn ~f:(+)

let () =
  In_channel.with_file "./2023/day13/input" ~f:(fun ch -> 
    let lines = In_channel.input_lines ch in
    Printf.printf "Day 13 - Answer 1: %d \n" (solve lines);
    (* Printf.printf "Day 13 - Answer 2: %d \n" (solve2 lines); *)
  )
