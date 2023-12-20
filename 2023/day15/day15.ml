open Core

let ch_to_ascii ch = Char.to_int ch

let hash str =
  str
  |> String.to_list
  |> List.fold_left ~init:0 ~f:(fun acc ch -> ((acc + (ch_to_ascii ch)) * 17) mod 256)

let get_steps input = 
  input
  |> List.hd_exn
  |> String.split ~on:','

let solve input = 
  input
  |> get_steps
  |> List.map ~f:hash
  |> List.reduce_exn ~f:(+)

let get_op step = 
  if String.contains step '=' 
  then (
    let [@warning "-8"][label; len] = String.split step ~on:'=' in
    (hash label, label, '=', Int.of_string len)
  ) else (
    let [@warning "-8"][label; _] = String.split step ~on:'-' in
    (hash label, label, '-', 0)
  )

let create_hashtbl = 
  let list = List.range 0 256 |> List.map ~f:(fun key -> (key, [])) in
  Hashtbl.of_alist_exn (module Int) list

let add_label alist label len = 
  if List.Assoc.mem alist ~equal:String.equal label
    then List.fold_left alist ~init:[] ~f:(fun acc (l, ln) -> 
      if String.equal l label then List.append acc [(l, len)] else List.append acc [(l, ln)]
    )
    else List.Assoc.add alist ~equal:String.equal label len

let remove_label alist label = List.Assoc.remove alist ~equal:String.equal label

let process_step boxes n label op len = 
  Hashtbl.update boxes n ~f:(fun alist -> 
    match alist, op with
    | Some [], '-' -> []
    | Some [], '=' -> [(label, len)]
    | Some al, '-' -> remove_label al label
    | Some al, '=' -> add_label al label len
    | None, _ | Some _, _ -> failwith "Impossibru"
  )

let score n lenses = 
  lenses
  |> List.rev
  |> List.mapi ~f:(fun i (_, len) -> (n + 1) * (i + 1) * len)
  |> List.reduce ~f:(+)
  |> Option.value ~default:0

let solve2 input = 
  let steps = input
    |> get_steps
    |> List.map ~f:(get_op) 
  in
  let boxes = create_hashtbl in
  let () = List.iter steps ~f:(fun (n, label, op, len) -> process_step boxes n label op len) in
  boxes
    |> Hashtbl.to_alist
    |> List.map ~f:(fun (n, lenses) -> score n lenses)
    |> List.reduce_exn ~f:(+)

let () =
  In_channel.with_file "./2023/day15/input" ~f:(fun ch -> 
    let lines = In_channel.input_lines ch in
    Printf.printf "Day 15 - Answer 1: %d \n" (solve lines);
    Printf.printf "Day 15 - Answer 2: %d \n" (solve2 lines);
  )
