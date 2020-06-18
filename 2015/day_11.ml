(* passwords *)
open Batteries;;
let (letters, base) =
  let s = String.explode "abcdefghijklmnopqrstuvwxyz" in
  (s, List.length s);;
type password = int list;;

let index_of c =
  let rec aux i = function
      []     -> None
    | hd::tl -> if hd = c then
                 Some i
               else
                 aux (i + 1) tl in
  aux 0 letters;;

let illegal_character c = c = 8 || c = 11 || c = 14;;

let string_of_password pwd =
  let string_index i = Char.escaped (List.nth letters i) in
  List.fold_left (^) "" (List.map string_index pwd);;

let password_of_string str =
  List.map (fun d -> match (index_of d) with
                    None -> assert false
                  | Some v -> v) (String.explode str);;

let has_illegal_characters = List.exists illegal_character;;

let has_straight pwd =
  let rec aux = function
      ([]|[_]|[_;_]) -> false
    | a::(b::c::_ as rest) -> if a = (b - 1) && b = (c - 1) then
                               true
                             else
                               aux rest
  in
  aux pwd;;

type state = Start | SeenFirst of int | HasFirst of int | SeenSecond of int | HasSecond of int;;
let has_two_pair pwd =
  let rec aux state list =
    match state, list with
      HasSecond _ , _ -> true
    | _, []           -> false
    | s, (hd::tl)     ->
       let new_state = (match s with
                          Start -> SeenFirst hd
                        | SeenFirst i when i = hd -> HasFirst i
                        | SeenFirst _             -> SeenFirst hd

                        | HasFirst i when i = hd -> HasFirst i
                        | HasFirst _             -> SeenSecond hd

                        | SeenSecond i when i = hd -> HasSecond i
                        | SeenSecond _             -> SeenSecond hd

                        | HasSecond i -> HasSecond i) in
       aux new_state tl
  in
  aux Start pwd;;

type pwd_t = Original of password | Truncated of password;;
let trunc pwd =
  let rec aux acc td = function
      [] -> if td then Truncated (List.rev acc) else Original (List.rev acc)
    | hd::tl when illegal_character hd && not td ->
       aux ((hd + 1)::acc) true tl
    | _::tl when td ->
       aux (0::acc) true tl
    | hd::tl ->
       aux (hd::acc) td tl
  in
  aux [] false pwd;;

let incr pwd =
  let rec aux acc carry = function
      [] -> if carry then 1::acc else acc
    | d::ds -> let (d', carry') =
                let d'' = d + (if carry then 1 else 0) in
                if d'' = base then
                  0, true
                else
                  d'', false
              in
              aux (d'::acc) carry' ds
  in
  match (trunc pwd) with Truncated p -> p
                       | Original  p -> aux [] true (List.rev p);;

let legal_pwd p = not (has_illegal_characters p) && has_two_pair p && has_straight p;;

let next_legal_pwd str =
  let pwd = password_of_string str in
  let rec aux p =
    if legal_pwd p then (string_of_password p)
    else aux (incr p)
  in
  aux pwd;;

let () =
  let pwd = "cqjxxyzz" in
  let a1 = next_legal_pwd pwd in
  let a2 = next_legal_pwd (string_of_password (incr (password_of_string a1))) in
  print_endline (Printf.sprintf "1: %s\n2: %s\n" a1 a2);;
