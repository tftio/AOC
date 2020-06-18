(* lights *)

open Batteries
type light = (int * int);;
module LM = Hashtbl.Make(
                struct
                  type t = light
                  let equal i j = match (i, j) with
                      (x1, y1), (x2, y2) -> x1 = x2 && y1 = y2
                  let hash = Hashtbl.hash
                end);;

let rect_to_list (x1, y1) (x2, y2) =
  let rec range a b =
    if a > b then []
    else a :: range (a + 1) b in
  List.concat (List.map (fun e -> List.map (fun e' -> (e,e')) (range y1 y2)) (range x1 x2));;

let find grid x y =
  try
    Some (LM.find grid (x, y))
  with
    _ -> None;;

let turn_off_1 grid x y = LM.replace grid (x, y) 0;;
let turn_on_1  grid x y = LM.replace grid (x, y) 1;;
let toggle_1   grid x y = LM.replace grid (x, y) (match (find grid x y) with
                                                  | Some v when v = 1 -> 0
                                                  | _                 -> 1);;

let turn_off_2 grid x y =
  LM.replace grid (x, y) (match (find grid x y) with
                          | Some v when v >= 1 -> (v - 1)
                          | _ -> 0);;
let turn_on_2 grid x y =
  LM.replace grid (x, y) (match (find grid x y) with
                            Some v -> v + 1
                          | _ -> 1);;
let toggle_2 grid x y =
  LM.replace grid (x, y) (match (find grid x y) with
                            Some v -> v + 2
                          | _ -> 2);;

let answer grid =
  let total : int ref = ref 0 in
  LM.iter (fun k v -> total := !total + v) grid;
  !total;;

let apply_line grid (f, tl, br) =
  List.iter (fun (x, y) -> f grid x y) (rect_to_list tl br);;

let apply grid input =
  List.iter (fun l -> apply_line grid l) input;
  grid;;

let everything = LM.create (1000 * 1000);;

let toggle = toggle_1;;
let turn_off = turn_off_1;;
let turn_on = turn_on_1;;

let input = [
(turn_off,(660,55),(986,197));
(turn_off,(341,304),(638,850));
(turn_off,(199,133),(461,193));
(toggle,(322,558),(977,958));
(toggle,(537,781),(687,941));
(turn_on,(226,196),(599,390));
(turn_on,(240,129),(703,297));
(turn_on,(317,329),(451,798));
(turn_on,(957,736),(977,890));
];;

let () =
  let answer_2 = answer (apply everything input) in
  print_string (Printf.sprintf "%d\n" answer_2);;
