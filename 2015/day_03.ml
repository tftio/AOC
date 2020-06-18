open Batteries

let moves = String.explode "v>v";; 

module SS = Set.Make(String);;
let compute_moves moves =
  let move_to (x, y) = function
      '^' -> (x, (y + 1))
    | 'v' -> (x, (y - 1))
    | '>' -> ((x + 1), y)
    | '<' -> ((x - 1), y)
    | _   -> (x, y) in

  let to_s x y = Printf.sprintf "%d,%d" x y in
  let rec move' (x,y) history = function
      [] -> SS.add (to_s x y) history
    | m::ms -> let (x', y') = move_to (x, y) m in
              move' (x', y') (SS.add (to_s x y) history) ms
  in
  move' (0, 0) SS.empty moves;;

let answer_1 = SS.cardinal (compute_moves moves);;
let answer_2 =
  let split_in_half l =
    let rec s' ct l1 l2 = function
        [] -> (l1, l2)
      | hd::tl -> if (ct mod 2) = 0 then
                  s' (ct + 1) (l1 @ (hd::[])) l2 tl
                else
                  s' (ct + 1) l1 (l2 @ (hd::[])) tl
    in
    s' 0 [] [] l in
  let (santa, robot) = split_in_half moves in
  SS.cardinal (SS.union (compute_moves santa)
                        (compute_moves robot));;
  
