open Core

let matrix_get matrix i j = 
  if i < 0 || i >= Array.length matrix || j < 0 || j >= Array.length matrix.(0) then None
  else Some matrix.(i).(j)

let neighbours matrix i j = 
  [
    matrix_get matrix (i - 1) (j - 1);
    matrix_get matrix (i - 1) (j);
    matrix_get matrix (i - 1) (j + 1);
    matrix_get matrix (i) (j - 1);
    matrix_get matrix (i) (j + 1);
    matrix_get matrix (i + 1) (j - 1);
    matrix_get matrix (i + 1) (j);
    matrix_get matrix (i + 1) (j + 1);
  ]
  |> List.filter_map ~f:(fun x -> x)

let symbols = ['#'; '$'; '%'; '&'; '*'; '+'; '-'; '/'; '='; '@']

let is_symbol ch = 
  List.mem symbols ch ~equal:Char.equal

let is_star ch = Char.equal ch '*'

let neighbours_has_sym matrix i j =
  neighbours matrix i j
  |> List.find ~f:is_symbol
  |> Option.is_some

let neighbours_has_star matrix i j =
  neighbours matrix i j
  |> List.find ~f:is_star
  |> Option.is_some

let lines_to_matrix input = 
  input
  |> Array.of_list
  |> Array.map ~f:String.to_array

let map_elements_with_bool matrix ~check = 
  matrix 
  |> Array.mapi ~f:(fun i row -> 
      Array.mapi row ~f:(fun j elem -> 
        (elem, check matrix i j)))

let group_into_numbers line = 
  Array.to_list line
  |> List.group ~break:(fun (a, _) (b, _) -> not (Char.is_digit a) || not (Char.is_digit b))

let filter_connected_numbers matrix =
  matrix
  |> Array.map ~f:(fun groups -> 
    groups
    |> List.filter ~f:(fun list -> 
      List.for_all list ~f:(fun (ch, _) -> Char.is_digit ch) && not (List.for_all list ~f:(fun (_, b) -> not b)))
    |> List.map ~f:(fun list -> List.map list ~f:(fun (ch, _) -> ch)))
  |> Array.filter ~f:(fun list -> not (List.is_empty list))
  |> Array.reduce_exn ~f:List.append
  |> List.map ~f:String.of_char_list
  |> List.map ~f:Int.of_string

let count_part_numbers input =
  lines_to_matrix input
  |> map_elements_with_bool ~check:neighbours_has_sym
  |> Array.map ~f:group_into_numbers
  |> filter_connected_numbers
  |> List.reduce_exn ~f:(+)

let window matrix i j = 
  [|
    Array.slice matrix.(i - 1) (j-3) (j+4);
    Array.slice matrix.(i) (j-3) (j+4);
    Array.slice matrix.(i + 1) (j-3) (j+4);
  |]

let remove_other_stars matrix: char array array = 
  matrix
  |> Array.mapi ~f:(fun i row -> 
      Array.mapi row ~f: (fun j elem -> 
        if (Char.equal elem '*') && (i <> 1 || j <> 3) then '.' else elem))

let check_engine matrix i j = 
  let engine_numbers = window matrix i j
  |> remove_other_stars
  |> map_elements_with_bool ~check:neighbours_has_star
  |> Array.map ~f:group_into_numbers
  |> filter_connected_numbers in

  match engine_numbers with
  | a :: b :: [] -> a * b
  | _ -> 0

let find_engines input = 
  let matrix = lines_to_matrix input in
  matrix
  |> Array.mapi ~f:(fun i row -> 
        Array.mapi row ~f: (fun j elem -> 
          if Char.equal elem '*' then check_engine matrix i j else 0))
  |> Array.map ~f:(fun row -> Array.reduce_exn row ~f:(+))
  |> Array.reduce_exn ~f:(+)

let main () =
  In_channel.with_file "./2023/day3/input.txt" ~f: (fun ch -> 
    let lines = In_channel.input_lines ch in
    Printf.printf "Day 3 - Answer 1: %d \n" (count_part_numbers lines);
    Printf.printf "Day 3 - Answer 1: %d \n" (find_engines lines)
  );;


main ();;