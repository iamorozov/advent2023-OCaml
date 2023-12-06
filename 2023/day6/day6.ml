open Core

let win_race time record =
  List.range 0 time
  |> List.map ~f:(fun t -> (time - t) * (t))
  |> List.filter ~f:(fun x -> x > record)
  |> List.length

let win_long_race time record =
  let result = ref 0 in 
  let () = for t = 0 to time do
    if ((time - t) * t) > record then result := !result + 1 else ()
  done in
  !result

let extract_collection line = 
  line
  |> String.split ~on:':'
  |> List.last_exn
  |> String.strip
  |> String.split ~on:' '
  |> List.filter ~f:(fun s -> not (String.equal s ""))
  |> List.map ~f:Int.of_string

let total_win_race input = 
  let times = extract_collection (List.hd_exn input) in
  let records = extract_collection (List.last_exn input) in
  List.zip_exn times records
  |> List.map ~f:(fun (time, record) -> win_race time record)
  |> List.reduce_exn ~f:(fun a b -> a * b)

let extract_long_race line = 
  line
  |> String.split ~on:':'
  |> List.last_exn
  |> String.filter ~f:(Char.is_digit)
  |> Int.of_string

let long_race input = 
  let time = extract_long_race (List.hd_exn input) in
  let record = extract_long_race (List.last_exn input) in
  win_long_race time record

let main () =
  In_channel.with_file "./2023/day6/input.txt" ~f: (fun ch -> 
    let lines = In_channel.input_lines ch in
    Printf.printf "Day 6 - Answer 1: %d \n" (total_win_race lines);
    Printf.printf "Day 6 - Answer 2: %d \n" (long_race lines);
  );;


main ();;