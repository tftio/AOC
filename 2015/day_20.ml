(* houses *)

open Batteries

module Houses = struct
  let target = 36000000

let factors n = 
  let small_factors =
    let root = int_of_float (sqrt (float_of_int n)) in
    let rec aux acc = function
        i when i = root -> i::acc
      | i when n mod i = 0 -> aux (i::acc) (i + 1)
      | i -> aux acc (i + 1)
    in
    aux [] 1
  in
  let large_factors =
    List.map (fun i -> n / i) (List.filter (fun i -> not (i * i = n) && i > 0) small_factors)
  in
  small_factors @ large_factors

  let answer_01 () =
    let presents_for n = 10 * (List.fold_left (+) 0 (factors n)) in
    let rec aux = function
      i when (presents_for i) >= target -> i
      | i -> aux (i + 1)
    in
    aux 100;;

  let answer_02 () =
    let presents_for n = 11 * (List.fold_left (+) 0 (List.filter (fun i -> i * 50 > n) (factors n))) in
    let rec aux = function
        i when (presents_for i) >= target -> i
      | i -> aux (i + 1)
    in
    aux 100;;
end

let () =
  print_endline (Printf.sprintf "Answer 01: %d" (Houses.answer_01()));
  print_endline (Printf.sprintf "Answer 01: %d" (Houses.answer_02()));
