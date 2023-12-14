open Core

let add opt1 opt2 = 
  match opt1, opt2 with
  | None, x | x, None -> x
  | Some(x), Some(y) -> Some(x + y)

let has_hash list = 
  List.exists list ~f:(Char.equal '#')
  
let rec arrange cond groups flag = 
  match cond, groups, flag with
  | xs, [], _ | xs, [0], _ when has_hash xs -> None
  | _, [], _ | _, [0], _-> Some 1
  | [], _, _ -> None
  | '.' :: cond_tail, _, false -> arrange cond_tail groups false
  | '.' :: cond_tail, 0 :: group_tail, true -> arrange cond_tail group_tail false
  | '.' :: cond_tail, group_hd :: group_tail, true -> None
  | '#' :: cond_tail, 0 :: group_tail, _ -> None
  | '#' :: cond_tail, group_hd :: group_tail, _ -> arrange cond_tail ((group_hd - 1) :: group_tail) true
  | '?' :: cond_tail, _, _ -> add (arrange ('.' :: cond_tail) groups flag) (arrange ('#' :: cond_tail) groups flag)
  | _, _, _ -> failwith "Impossible"

let extract line = 
  let [cond; groups] = String.split ~on:' ' line in
  let cond_l = String.to_list cond in
  let groups_l = String.split groups ~on:',' |> List.map ~f:Int.of_string in
  (cond_l, groups_l)

let solve lines =
  lines
  |> List.map ~f:(fun line -> 
      let cond, group = extract line in
      arrange cond group false
    )
  |> List.reduce_exn ~f:add
  |> Option.value_exn

let five_lines line nums =
  ( line ^ "?" ^ line ^ "?" ^ line ^ "?" ^ line ^ "?" ^ line,
    nums @ nums @ nums @ nums @ nums )

let extract5 line = 
  let [cond; groups] = String.split ~on:' ' line in
  let groups_l = String.split groups ~on:',' |> List.map ~f:Int.of_string in
  let (cond5, group5) = five_lines cond groups_l in
  (String.to_list cond5, group5)

let solve2 lines =
  lines
  |> List.map ~f:(fun line -> 
      let cond, group = extract5 line in
      arrange cond group false
    )
  |> List.reduce_exn ~f:add
  |> Option.value_exn

let () =
  In_channel.with_file "./2023/day12/input" ~f: (fun ch -> 
    let lines = In_channel.input_lines ch in
    Printf.printf "Day 12 - Answer 1: %d \n" (solve lines);
    Printf.printf "Day 12 - Answer 2: %d \n" (solve2 lines);
  )
