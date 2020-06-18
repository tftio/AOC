open Batteries;;

module H = Map.Make(struct
                     type t = string * string
                     let compare (a, b) (a', b') =
                       match Pervasives.compare a a' with
                         0 -> Pervasives.compare b b'
                       | c -> c
                   end);;

module S = Set.Make(String);;

let file_as_lines name = BatEnum.fold (fun acc l -> l::acc) [] (File.lines_of name);;
let rec permutation ps =
  let distribute c l =
    let rec insert acc1 acc2 = function
      | [] -> acc2
      | hd::tl ->
         insert (hd::acc1) ((List.rev_append acc1 (hd::c::tl)) :: acc2) tl
    in
    insert [] [c::l] l in
  match ps with
  | [] -> [[]]
  | hd::tl ->
     List.fold_left (fun acc x -> List.rev_append (distribute hd x) acc) [] (permutation tl);;

let seating_from_line (names, pairs) line =
  let ls = String.nsplit line " " in
  let first = List.nth ls 0 in
  let last  = String.sub (List.nth ls 10) 0 ((String.length (List.nth ls 10)) - 1) in
  let happy = (match (List.nth ls 2) with
                 "gain" -> 1  |
                 "lose" -> -1 |
                 _ -> assert false) *
                int_of_string (List.nth ls 3) in
  S.add last (S.add first names), H.add (first, last) happy pairs;;

let names, seatings = List.fold_left seating_from_line (S.empty, H.empty) (file_as_lines "day_13.input");;

let sum names =
  let pair ns =
    let h = List.hd ns in
    let rec aux acc = function
        [] -> acc
      | [a] -> (a, h)::acc
      | a::(b::_ as tl) -> aux ((a,b)::acc) tl
    in
    aux [] ns in
  let happy (f, l) = try (H.find (f, l) seatings) + (H.find (l, f) seatings)
                     with Not_found -> 0 in
  List.fold_left (fun sum p -> sum + happy p) 0 (pair names);;

let answer n = List.hd (List.sort (fun a b -> Pervasives.compare b a) (List.map sum (permutation n)));;
let a1, a2 = answer (S.elements names), answer (S.elements (S.add "Me" names));;
