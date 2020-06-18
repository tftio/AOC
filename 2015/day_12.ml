(* json *)

#require "yojson" ;;
open Yojson;;
let json = Yojson.Basic.from_file "day_12.input";;

let sum json =
  let rec aux acc = function
      `Int i -> acc + i
    | `List js  -> List.fold_left aux acc js
    | `Assoc js -> List.fold_left (fun s (_, v) -> aux s v) acc js
    | _ -> acc
  in aux 0 json;;

let strip json =
  let has_red_value =
    List.exists
      (fun (k, v) -> match v with
                    `String r when r = "red" -> true
                  | _ -> false)
  in
  let rec aux = function
      `Assoc js -> if has_red_value js then
                    `Null
                  else
                    `Assoc (List.map (fun (k, v) -> (k, aux v)) js)
    | `List js  -> `List (List.map aux js)
    | (`Int _ | `String _ | `Bool _ | `Float _ | `Null) as a -> a
  in
  aux json;;

let answer01, answer02 = sum json, sum (strip json);;
