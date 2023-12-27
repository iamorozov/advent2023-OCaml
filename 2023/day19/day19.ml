open Core
open Angstrom

(* Data types *)

type part = { x: int; m: int; a: int; s: int }
let create_part x m a s = {x; m; a; s}

type dest = 
  | Workflow of string
  | R
  | A

type rule = 
  | Check of string * (int -> int -> bool) * int * dest
  | Terminal of dest

(* Parsers *)

let integer =
  take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

let name = take_while1 (function 'a'..'z' -> true | _ -> false)

let parens p = char '{' *> p <* char '}'

let comma = char ','

let gt = char '>' *> return (>)

let lt = char '<' *> return (<)

let sign = gt <|> lt

let field = advance 1 *> char '=' *> integer

let part_parser = create_part <$> char '{' *> field <* comma <*> field <* comma <*> field <* comma <*> field <* char '}'

let dest_parser = name <|> string "R" <|> string "A"
  >>= function
  | "R" -> return R
  | "A" -> return A
  | s -> return (Workflow(s))

let check_rule_parser = name
  >>= fun n -> sign 
  >>= fun check -> integer 
  >>= fun num -> char ':' *> dest_parser
  >>= fun dest -> return (Check(n, check, num, dest))

let terminal_rule_parser = dest_parser >>= fun d -> return (Terminal(d))

let rule_parser = check_rule_parser <|> terminal_rule_parser

let rules_parser = name 
  >>= fun n -> parens (sep_by1 comma rule_parser) 
  >>= fun rs -> return (n, rs)
  
let parse_expr parser s = 
  match parse_string ~consume:All parser s with
  | Ok v      -> v
  | Error msg -> failwith msg

(* Solution *)

let parse input = 
  let [rules; parts] = List.group input ~break:(fun a _ -> String.equal a "") in
  let parsed_rules = rules 
    |> List.filter ~f:(fun r -> not (String.is_empty r))
    |> List.map ~f:(parse_expr rules_parser)
    |> Hashtbl.of_alist_exn (module String) in
  let parsed_parts = parts |> List.map ~f:(parse_expr part_parser) in
  (parsed_rules, parsed_parts)

let get_field f part = 
  match f with
  | "x" -> part.x
  | "m" -> part.m
  | "a" -> part.a
  | "s" -> part.s

let process rules part = 
  let rec loop workflow =
    let navigate = function
    | Workflow(s) -> loop (Hashtbl.find_exn rules s)
    | result -> result in

    match workflow with
    | [] -> failwith "impossible"
    | r :: rs -> match r with
      | Terminal dest -> navigate dest
      | Check(field, check, num, dest) ->
        if check (get_field field part) num then navigate dest
        else loop rs
    in
  loop (Hashtbl.find_exn rules "in")

let solve input =
  let rules, parts = parse input in
  List.map parts ~f:(fun part -> 
    match process rules part with
    | A -> part.x + part.m + part.a + part.s
    | R -> 0
  )
  |> List.reduce_exn ~f:(+)

let solve2 input = 
  0

let () =
  In_channel.with_file "./2023/day19/input" ~f:(fun ch -> 
    let lines = In_channel.input_lines ch in
    Printf.printf "Answer 1: %d \n" (solve lines);
    Printf.printf "Answer 2: %d \n" (solve2 lines);
  )
