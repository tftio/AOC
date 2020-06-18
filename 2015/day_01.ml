(* count parens *)
open Batteries
let input = "()(";;

let char_to_floor = function
    '(' -> 1
  | ')' -> -1
  | _   -> 0;;

let count s =
  let rec count c = function
      [] -> c
    | hd::tl -> count (c + (char_to_floor hd)) tl in
  count 0 (String.explode s);;

let first s =
  let rec first floor position = function
      [] -> None
    | hd::tl -> let incr = char_to_floor hd in
               if (floor = 0) && (incr = -1) then
                 Some position
               else
                 first (floor + incr) (position + 1) tl in
  first 0 1 (String.explode s);;
