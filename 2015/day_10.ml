open Batteries;;

let encode ss =
  let rec aux previous counter acc = function
        []    -> List.rev (previous::(string_of_int counter)::acc)
      | d::ds when (d = previous) -> aux previous (counter + 1) acc ds
      | d::ds -> aux d 1 (previous::(string_of_int counter)::acc) ds
  in
  aux (List.hd ss) 1 [] (List.tl ss);;

let answer iter start =
  let l = ref (List.map Char.escaped (String.explode start)) in
  for i = 1 to iter do
    l := encode !l;
  done;
  List.length !l;;

let () = print_endline (string_of_int (answer 50 "1321131112"));;
