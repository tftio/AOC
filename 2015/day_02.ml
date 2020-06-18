(* wrapping paper *)

open Batteries;;

let s_to_l s = List.map (Int.of_string) (String.nsplit s "x");;

let input = List.map s_to_l (String.nsplit "4x23x21
22x29x19
11x4x11" "\n");; 

let wrap h w l =
  let lw = l * w in
  let wh = w * h in
  let hl = h * l in
  let smallest = min lw (min wh hl) in
  smallest + (2 * lw) + (2 * wh) + (2 * hl);;

let ribbon h w l =
  let lw = l + w in
  let wh = w + h in
  let hl = h + l in
  let perimeter = min lw (min wh hl) in
  let bow = l * w * h in
  (2 * perimeter) + bow;;

exception BogusList;;
let unpack f = function
    (a::b::c::[]) -> f a b c
  | _  -> raise BogusList;;

let ok = (wrap 1 1 10) = 43;;
let ok' = (wrap 2 3 4) = 58;;

let all_ribbon = List.fold_left (fun a e -> (a + ((unpack ribbon) e))) 0 input;;
let paper = List.fold_left (fun a e -> (a + ((unpack wrap) e))) 0 input;;


