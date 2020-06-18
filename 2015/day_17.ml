open Batteries;;

module Eggnog = struct
  let combinations containers max =
    let rec aux acc cs remaining = match (cs, remaining) with
        _, 0 -> [acc]
      | [], _ -> []
      | hd::tl, _ -> (aux (hd::acc) tl (remaining - hd)) @
                      (aux acc tl remaining)
    in
    aux [] containers max

  let answer_01 c m = List.length (combinations c m)
  let answer_02 c m =
    let find_count_of_min = function
        [] -> (0, 0)
      | hd::lists ->
         let init = (List.length hd, 1) in
         let count (length, count) list =
           let length' = List.length list in
           match length' with
             _ when length = length' -> (length, (count + 1))
           | _ when length > length' -> (length', 1)
           | _ -> (length, count)
         in
         List.fold_left count init lists
    in
    find_count_of_min (combinations c m)
end;;
let sizes = List.rev (List.sort Pervasives.compare [11;30;47;31;32;36;3;1;5;3;32;36;15;11;46;26;28;1;19;3]);;
let max   = 150;;
let answer_01 = Eggnog.answer_01 sizes 150;;
let answer_02 = Eggnog.answer_02 sizes 150;;
