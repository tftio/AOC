(* circuits *)

open Batteries
type argument = Gate of string | Literal of int;;
module ENV = Hashtbl.Make(
                 struct
                   type t = string
                   let equal a b = a = b
                   let hash = Hashtbl.hash
                 end
               );;

type operator = Not of argument |
                Assign of argument |
                Or of argument * argument |
                And of argument * argument |
                LShift of argument * argument |
                RShift of argument * argument ;;

type instruction = argument * operator;;

let parse_instruction line =
  let parse_arg a =
    try
      Literal (int_of_string a)
    with
      Failure _ -> Gate a
  in
  let line' = String.nsplit line " " in
  let target_gate = List.hd (List.rev line') in
  let lhs = List.rev (List.tl (List.tl (List.rev line'))) in
  let op = match (List.length lhs) with
      1 -> Assign (parse_arg (List.nth lhs 0))
    | 2 -> let op = List.nth lhs 0 in
          let a = parse_arg (List.nth lhs 1) in
          (match op with
             "NOT" -> Not a
           | _     -> assert false)
    | 3 -> let a  = parse_arg (List.nth lhs 0) in
          let op = List.nth lhs 1 in
          let b  = parse_arg (List.nth lhs 2) in
          (match op with
             "LSHIFT" -> LShift (a, b)
           | "RSHIFT" -> RShift (a, b)
           | "AND"    -> And (a, b)
           | "OR"     -> Or  (a, b)
           | _        -> assert false)
    | _ -> assert false in
  (target_gate, op);;

let dependencies_of (target, op) =
  let summarize = function
      Literal _ -> None
    | Gate    g -> Some g in
  (target, List.map (fun v -> match v with Some v' -> v' | _ -> assert false)
                    (List.filter (fun v -> match v with None -> false | _ -> true)
                                 (match op with
                                    Assign _ -> []
                                  | Not g (   -> [summarize g]
                                  | (And    (g, g') |
                                     Or     (g, g') |
                                     LShift (g, g') |
                                     RShift (g, g')) -> [summarize g; summarize g'])));;

let parse_instructions instructions =
  let instructions' = List.map parse_instruction instructions in
  let defs = ENV.create (List.length instructions') in
  let deps = List.map dependencies_of instructions' in
  List.iter (fun (target_gate, op) -> ENV.replace defs target_gate op) instructions';
  (defs, deps);;

let toposort graph =
  let dfs graph visited start_node =
    let rec explore path visited node =
      if List.mem node path    then assert false else
        if List.mem node visited then visited else
          let new_path = node :: path in
          let edges    = List.assoc node graph in
          let visited  = List.fold_left (explore new_path) visited edges in
          node :: visited
    in explore [] visited start_node
  in
  List.rev (List.fold_left (fun visited (node,_) -> dfs graph visited node) [] graph);;

let solve input =
  let (defs, deps) = parse_instructions input in
  let env = ENV.create (List.length deps) in
  let getenv a = match a with
      Literal l -> l
    | Gate g    -> ENV.find env g in
  let ordered_instructions =
    List.map (fun d -> (d, ENV.find defs d)) (toposort deps) in
  let lookup = function
      Assign a  -> getenv a
    | Not    a  -> lnot (getenv a)
    | Or  (a, b) -> (getenv a) lor (getenv b)
    | And (a, b) -> (getenv a) land (getenv b)
    | RShift (a, b) -> (getenv a) lsr (getenv b)
    | LShift (a, b) -> (getenv a) lsl (getenv b) in
  List.iter (fun (g, def) -> ENV.replace env g (lookup def))
            ordered_instructions;
  env;;

let file_as_lines name = BatEnum.fold (fun acc l -> l::acc) [] (File.lines_of name);;
let answer_01 = solve (file_as_lines "day_07.input");;
let answer_02 = solve (file_as_lines "day_07_2.input");;
