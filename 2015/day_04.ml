(* md5 *)

open Batteries
let key = "bgvyzdsv";;

let find_key key len =
  let match_p hash = (String.sub hash 0 len) = (String.make len '0') in
  let get_hash c = Digest.to_hex (Digest.string (key ^ Int.to_string c)) in
  let rec find_key c =
    let h = get_hash c in
    if match_p h then
      c
    else
      find_key (c + 1) in
  find_key 0;;

let part_1 = find_key key 5;;
let part_2 = find_key key 6;;
