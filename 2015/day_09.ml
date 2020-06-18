open Batteries

module H = Hashtbl.Make(struct
                         type t = string * string
                         let equal (a1, a2) (b1, b2) = ((a1 = b1) && (a2 = b2)) || ((a2 = b1) && (a1 = b2))
                         let hash = Hashtbl.hash
                       end);;
module S = Set.Make(String);;

let file_as_lines name = List.rev (List.map (fun s -> String.sub s 1 (String.length s - 2)) (BatEnum.fold (fun acc l -> l::acc) [] (File.lines_of name)));;

let parse_line l =
  let l' = (String.nsplit l " ") in
  (List.nth l' 0, List.nth l' 2, int_of_string (List.nth l' 4));;

let (distances, cities) =
  let lines = (List.map parse_line (BatEnum.fold (fun acc l -> l::acc) [] (File.lines_of "day_09.input"))) in
  let s = List.fold_left (fun acc (a, b, _) -> S.add a (S.add b acc)) S.empty lines in
  let g = H.create(10) in
  List.iter
    (fun (a, b, weight) -> H.add g (a, b) weight; H.add g (b, a) weight)
    lines;
  (g, S.elements s);;

let distribute c l =
  let rec insert acc1 acc2 = function
    | [] -> acc2
    | hd::tl ->
      insert (hd::acc1) ((List.rev_append acc1 (hd::c::tl)) :: acc2) tl
  in
  insert [] [c::l] l;;

let rec permutation = function
  | [] -> [[]]
  | hd::tl ->
    List.fold_left (fun acc x -> List.rev_append (distribute hd x) acc) [] (permutation tl);;

let distance cities =
  let all = H.fold (fun k v acc -> (k, v)::acc) distances [] in
  let rec aux acc = function
      ([] | [_]) -> acc
    | a::b::cs   -> let distance = List.assoc (a,b) all in
                   aux (acc + distance) (b::cs) in
  aux 0 cities;;

let all_distances = List.sort (fun (d, _) (d', _) -> compare d d') (List.map (fun c -> (distance c), c) (permutation cities));;
let shortest_distance = List.hd all_distances;;
let longest_distance  = List.hd (List.rev all_distances);;
