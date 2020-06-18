(* regexp *)
open Batteries

let input = String.nsplit "rthkunfaakmwmush" "\n";;

let (answer1, answer2) =
  let check r s = try (Str.search_forward r s 0) >= 0 with Not_found -> false in

  (* answer 1 *)
  let has_three_vowels = check (Str.regexp "[aeiou]+.*[aeiou]+.*[aeiou]+") in
  let doubled_letter   = check (Str.regexp "\\([a-z]\\)\\1") in
  let has_bogus_pair   = check (Str.regexp "\\(ab\\|cd\\|pq\\|xy\\)") in

  (* answer 2 *)
  let doubled_pair = check (Str.regexp "\\(..\\).*\\1") in
  let around       = check (Str.regexp "\\(.\\).\\1") in

  let a1 s = (has_three_vowels s) && (doubled_letter s) && not (has_bogus_pair s) in
  let a2 s = (doubled_pair s) && (around s) in

  List.fold_left (fun (x, y) s -> let x' = a1 s in
                               let y' = a2 s in
                               match (x', y') with
                                 true, true   -> ((x + 1), (y + 1))
                               | true, false  -> ((x + 1), y)
                               | false, true  -> (x,       (y + 1))
                               | false, false -> (x,       y))
                 (0, 0)
                 input;;
