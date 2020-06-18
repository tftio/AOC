open Batteries;;

module Aunt = struct
  type t = { number: int;
             children: int option;
              cats: int option;
              pomeranians: int option;
              samoyeds: int option;
              akitas: int option;
              vizslas: int option;
              goldfish: int option;
              trees: int option;
              cars: int option;
              perfumes: int option }
  let empty = { number = 0;
                children = None;
                cats = None;
                pomeranians = None;
                samoyeds = None;
                akitas = None;
                vizslas = None;
                goldfish = None;
                trees = None;
                cars = None;
                perfumes = None }
  let number i = i.number
  let children i = i.children
  let cats i = i.cats
  let pomeranians i = i.pomeranians
  let samoyeds i = i.samoyeds
  let akitas i = i.akitas
  let vizslas i = i.vizslas
  let goldfish i = i.goldfish
  let trees i = i.trees
  let cars i = i.cars
  let perfumes i = i.perfumes

  let exact_selectors = [children;samoyeds;akitas;vizslas;cars;perfumes]
  let less_than_selectors = [pomeranians;goldfish]
  let greater_than_selectors = [cats;trees]
  let comp' f' s a b =
    List.filter (fun a -> match a with None -> false | Some b -> b)
                (List.map (fun f -> match (f a), (f b) with
                                   (None, _|_, None) -> None
                                 | Some i, Some i' -> Some (f' i i')) s)

  let comp1 a b = comp' (=) (exact_selectors @ less_than_selectors @ greater_than_selectors) a b
  let comp2 a b = (comp' (=) exact_selectors a b)
                  @ (comp' (<) less_than_selectors a b)
                  @ (comp' (>) greater_than_selectors a b)

  exception Illegal_aunt of string

  let aunt_of_string str =
    let trim str =
      let l = String.length str in
      match (String.get str (l - 1)) with
        (':'|',') -> String.sub str 0 (l - 1)
      | _ -> str in
    let value v = Some (int_of_string (trim v)) in
    let rec aux a = function
        []  -> a
      | [_] -> raise (Illegal_aunt str)
      | k::v::tl ->
         match k with
         "Sue" -> aux { a with number = int_of_string (trim v) } tl
         | "children:" -> aux { a with perfumes = value v } tl
         | "cats:" -> aux { a with cats = value v } tl
         | "pomeranians:" -> aux { a with pomeranians = value v } tl
         | "samoyeds:" -> aux { a with samoyeds = value v } tl
         | "akitas:" -> aux { a with akitas = value v } tl
         | "vizslas:" -> aux { a with vizslas = value v } tl
         | "goldfish:" -> aux { a with goldfish = value v } tl
         | "trees:" -> aux { a with trees = value v } tl
         | "cars:" -> aux { a with cars = value v } tl
         | "perfumes:" -> aux { a with perfumes = value v } tl
         | _ -> aux a tl
    in
    aux empty (String.nsplit str " ")

  let similarity_scores fn aunt aunts =
    List.map (fun a -> (a, List.length (fn a aunt))) aunts

  let most_similar fn aunt aunts =
    let sort (_, a) (_, b) = Pervasives.compare b a in
    match (List.hd (List.sort sort (similarity_scores fn aunt aunts))) with
      a, _ -> a

end;;

let aunts = let file_as_lines name = BatEnum.fold (fun acc l -> l::acc) [] (File.lines_of name)
            in
            List.map Aunt.aunt_of_string (file_as_lines "day_16.input");;

let sue_0 = Aunt.aunt_of_string "Sue 0: children: 3, cats: 7, samoyeds: 2, pomeranians: 3, akitas: 0, vizslas: 0, goldfish: 5, trees: 3, cars: 2, perfumes: 1";;

let results_01 = Aunt.number (Aunt.most_similar Aunt.comp1 sue_0 aunts);;
let results_02 = Aunt.number (Aunt.most_similar Aunt.comp2 sue_0 aunts);;
