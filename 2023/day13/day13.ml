open Core

let lines_to_mat lines = 
  lines
  |> Array.map ~f:String.to_array

let group_patterns lines = 
  lines
  |> List.group ~break:(fun _ b -> String.equal "" b)
  |> List.map ~f:(fun p -> List.filter p ~f:(fun s -> not (String.equal "" s)))
  |> List.map ~f:List.to_array
  |> List.map ~f:lines_to_mat

let eq a1 a2 = 
  Array.equal Char.equal a1 a2

let find_mirror_row ?(skip_row = None) rows = 
  rows
  |> Array.findi ~f:(fun n _ -> 
    if (Option.is_some skip_row && Option.value_exn skip_row = n)
      then false
      else (
        let i = ref n in
        let j = ref (n + 1) in
        let len = Array.length rows in
        let is_mirror = ref false in
        while !i >= 0 && !j < len do
          if eq rows.(!i) rows.(!j) then (
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
  )
  |> Option.map ~f:(fun (i, _) -> i + 1)
  |> Option.value ~default:0

let transpose_mat mat =
  mat
  |> Array.transpose_exn

let solve input = 
  input
  |> group_patterns
  |> List.map ~f:(fun pattern ->
      let rows_mirror = find_mirror_row pattern in
      let cols_mirror = if rows_mirror = 0 then
        let pattern_t = transpose_mat pattern in
        find_mirror_row pattern_t
      else 0 in
      rows_mirror * 100 + cols_mirror
    )
  |> List.reduce_exn ~f:(+)

let flip sym = if Char.equal '.' sym then '#' else '.'

let find_other_mirror matrix row_m col_m = 
  let res = ref 0 in
  try
    for i = 0 to Array.length matrix - 1 do
      for j = 0 to Array.length matrix.(0) -1 do
        matrix.(i).(j) <- flip matrix.(i).(j);
        let new_mirror_row = find_mirror_row ~skip_row:(Some(row_m - 1)) matrix in
        if new_mirror_row <> 0 then (
          res := new_mirror_row * 100;
          raise Exit;
        ) else (
          let matrix_t = transpose_mat matrix in
          let new_mirror_col = find_mirror_row ~skip_row:(Some(col_m - 1)) matrix_t in
          if new_mirror_col <> 0 then (
            res := new_mirror_col;
            raise Exit;
          ) else (
            matrix.(i).(j) <- flip matrix.(i).(j);
          )
        )
      done
    done;
    !res
  with Exit -> ();
  !res

let solve2 input = 
  input
  |> group_patterns
  |> List.map ~f:(fun pattern ->
      let rows_mirror = find_mirror_row pattern in
      let pattern_t = transpose_mat pattern in
      let cols_mirror = find_mirror_row pattern_t in
      let res = find_other_mirror pattern rows_mirror cols_mirror in
      Printf.printf "row: %d col: %d alt: %d\n" rows_mirror cols_mirror res;
      res
    )
  |> List.reduce_exn ~f:(+)

let () =
  In_channel.with_file "./2023/day13/input" ~f:(fun ch -> 
    let lines = In_channel.input_lines ch in
    Printf.printf "Day 13 - Answer 1: %d \n" (solve lines);
    Printf.printf "Day 13 - Answer 2: %d \n" (solve2 lines);
  )
