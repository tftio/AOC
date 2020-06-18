(* cookies *)
open Batteries;;

module Ingredient = struct
  type t = { name: string;
             capacity: int;
             durability: int;
             flavor: int;
             texture: int;
             calories: int }
  let name i = i.name
  let capacity i = i.capacity
  let durability i = i.durability
  let flavor i = i.flavor
  let texture i = i.texture
  let calories i = i.calories
end;;
type recipie = (Ingredient.t * int) list;;

let ingredient_of_line line =
  let ls = String.nsplit line " " in
  let remove_last s = String.sub s 0 ((String.length s) - 1) in
  { Ingredient.
    name = remove_last (List.nth ls 0);
    capacity = int_of_string (remove_last (List.nth ls 2));
    durability = int_of_string (remove_last (List.nth ls 4));
    flavor = int_of_string (remove_last (List.nth ls 6));
    texture = int_of_string (remove_last (List.nth ls 8));
    calories = int_of_string (List.nth ls 10) };;

let ingredients = let file_as_lines name = BatEnum.fold (fun acc l -> l::acc) [] (File.lines_of name)
                  in
                  List.map ingredient_of_line (file_as_lines "day_15.input");;

let valid_recipie recipie =
  List.length recipie = 4 && (List.fold_left (fun acc (_, a) -> acc + a) 0 recipie) = 100;;

let score_of_recipie recipie =
  let s fn (i, amt) = (fn i) * amt in
  let calories = s Ingredient.calories in
  let s' fn rs = max 0 (List.fold_left (fun acc i -> acc + fn i) 0 rs) in
  let calories =
    List.fold_left (fun acc i -> acc + (calories i))
                   0
                   recipie in

  let score = List.fold_left
                ( * )
                1
                (List.map (fun f -> s' (s f) recipie) [Ingredient.capacity;
                                                    Ingredient.durability;
                                                    Ingredient.flavor;
                                                    Ingredient.texture]) in
  (calories, score);;

exception Mismatched_lists;;
let zip l1 l2 =
  let rec aux acc l1 l2 =
    match l1, l2 with
      [],[] -> acc
    | (_, [] | [], _) -> raise Mismatched_lists
    | hd::tl, hd'::tl' -> aux ((hd,hd')::acc) tl tl'
  in
  aux [] l1 l2;;

(* ugh *)
let make_all fn max =
  let all = ref [] in
  for i = 0 to max do
    for j = 0 to max - i do
      for k = 0 to max - i - j do
        for l = 0 to max - i - j - k do
          if i + j + k + l = 100 then
            all := (score_of_recipie (zip ingredients [i;j;k;l]))::!all
        done
      done
    done
  done;
  !all;;

let () = let all = List.sort (fun a b -> match a, b with (_, s), (_, s') -> Pervasives.compare s' s)
                             (make_all valid_recipie 100)
         in
         let answer_01 = match (List.hd all) with (_, s) -> s in
         let answer_02 = match (List.hd (List.filter (fun (c, _) -> c = 500) all)) with (_, s) -> s in
         print_endline ("Answer 1: " ^ (string_of_int answer_01) ^ "\nAnswer 2: " ^ (string_of_int answer_02));;
