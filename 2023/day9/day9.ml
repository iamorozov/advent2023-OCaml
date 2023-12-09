open Core

let all_equal seq =
  seq
  |> List.all_equal ~equal:Int.equal

let take_without_last list = 
  List.take list (List.length list - 1)

let line_to_list line = 
  line
  |> String.split ~on:' '
  |> List.map ~f:Int.of_string

let diff seq = 
  List.zip_exn (take_without_last seq) (List.tl_exn seq)
  |> List.map ~f:(fun (a, b) -> b - a)

let rec predict seq = 
  match all_equal seq with
  | Some e -> e
  | None -> 
    let diff = diff seq in
    predict diff + List.last_exn seq

let rec predict_first seq = 
  match all_equal seq with
  | Some e -> e
  | None -> 
    let diff = diff seq in
    (List.hd_exn seq) - predict_first diff

let solve input solver = 
  input
  |> List.map ~f:(line_to_list)
  |> List.map ~f:(solver)
  |> List.reduce_exn ~f:(+)

let () =
  In_channel.with_file "./2023/day9/input.txt" ~f: (fun ch -> 
    let lines = In_channel.input_lines ch in
    Printf.printf "Day 9 - Answer 1: %d \n" (solve lines predict);
    Printf.printf "Day 9 - Answer 2: %d \n" (solve lines predict_first);
  )
