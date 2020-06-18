(* stringy *)
open Batteries;;
let file_as_lines name = List.rev (List.map (fun s -> String.sub s 1 (String.length s - 2)) (BatEnum.fold (fun acc l -> l::acc) [] (File.lines_of name)));;
let strings = file_as_lines "day_08.input";;

exception Bogus_char of string * char;;

let escape chars =
  let rec aux acc = function
      []    -> List.rev acc
    | c::cs -> aux (match c with
                     '"' | '\\' as c -> [c;'\\'] @ acc
                     | c -> c::acc) cs in
  aux [] chars;;

let sizes s =
  let escaped_size s = 6 + (List.length (escape (String.explode s))) in
  let (b, l) =
    let rec aux b l escaped_p = function
        []    -> (b, l)
      | c::cs -> (match c with
                   '\\' -> if escaped_p then
                            aux (b + 1) (l + 1) false cs
                          else
                            aux (b + 1) l true cs
                 | '"'  -> aux (b + 1) (l + 1) false cs
                 | 'x'  -> if escaped_p then
                            aux (b + 3) (l + 1) false (List.tl (List.tl cs))
                          else
                            aux (b + 1) (l + 1) false cs
                 | c when escaped_p -> raise (Bogus_char (s, c))
                 | _ -> aux (b + 1) (l + 1) false cs)
    in
    aux 2 0 false (String.explode s)
  in
  (b, l, escaped_size s);;

let (answer_01, answer_02) = let (all_bytes, all_lengths, all_escaped) =
                               List.fold_left (fun (b, l, e) s -> let (b', l', e') = sizes s in
                                                               (b + b', l + l', e + e'))
                                              (0, 0, 0)
                                              strings
                             in
                             all_bytes - all_lengths,
                             all_escaped - all_bytes;;

