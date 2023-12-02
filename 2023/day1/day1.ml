open Core

let str_to_dig = List.zip_exn ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"] ["o1e"; "t2o"; "t3e"; "f4r"; "f5e"; "s6x"; "s7n"; "e8t"; "n9e"]

let rec replace_digits str i = 
  if i >= String.length str then str
  else
    let suffix = String.drop_prefix str i in
    let replacement = List.find str_to_dig ~f: (fun (s, _) -> String.is_prefix suffix ~prefix:s) in
    match replacement with
    | None -> replace_digits str (i + 1)
    | Some (s, d) -> 
        let new_str = String.substr_replace_first ~pos:i str ~pattern:s ~with_:d in
        replace_digits new_str (i + 1)

let count_digits_replace input =
  List.fold input ~init:0 ~f: (fun acc line ->
      let replaced = replace_digits line 0 in
      let digits = List.filter (String.to_list replaced) ~f:Char.is_digit in
      let value = String.of_char_list [List.hd_exn digits; List.last_exn digits] in
      acc + Int.of_string value
    )

let count_digits input =
  List.fold input ~init:0 ~f: (fun acc line ->
      let digits = List.filter (String.to_list line) ~f:Char.is_digit in
      let value = String.of_char_list [List.hd_exn digits; List.last_exn digits] in
      acc + Int.of_string value
    )


let main () =
  In_channel.with_file "./2023/day1/input.txt" ~f: (fun ch -> 
    let lines = In_channel.input_lines ch in
    Printf.printf "Day 1 - Answer 1: %d \n" (count_digits lines);
    Printf.printf "Day 1 - Answer 2: %d \n" (count_digits_replace lines)
  );;


main ();;