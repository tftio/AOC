(* Medicine *)

# require "pcre";;

open Batteries
open Pcre

module Medicine = struct
  type atom = string
  type molecule = atom list
  type replacement = molecule * molecule

  module S = Set.Make(String)

  let substitutions = [("Al", "ThF"); ("Al", "ThRnFAr"); ("B", "BCa"); ("B", "TiB");
                       ("B", "TiRnFAr"); ("Ca", "CaCa"); ("Ca", "PB"); ("Ca", "PRnFAr");
                       ("Ca", "SiRnFYFAr"); ("Ca", "SiRnMgAr"); ("Ca", "SiTh"); ("F", "CaF");
                       ("F", "PMg"); ("F", "SiAl"); ("H", "CRnAlAr"); ("H", "CRnFYFYFAr");
                       ("H", "CRnFYMgAr"); ("H", "CRnMgYFAr"); ("H", "HCa"); ("H", "NRnFYFAr");
                       ("H", "NRnMgAr"); ("H", "NTh"); ("H", "OB"); ("H", "ORnFAr");
                       ("Mg", "BF"); ("Mg", "TiMg"); ("N", "CRnFAr"); ("N", "HSi");
                       ("O", "CRnFYFAr"); ("O", "CRnMgAr"); ("O", "HP"); ("O", "NRnFAr");
                       ("O", "OTi"); ("P", "CaP"); ("P", "PTi"); ("P", "SiRnFAr");
                       ("Si", "CaSi"); ("Th", "ThCa"); ("Ti", "BP"); ("Ti", "TiTi")]

                      (*                       ("e", "HF"); ("e", "NAl"); ("e", "OMg")] *)

  let atoms_of_string molecule =
    let is_upper c = let c' = Char.code c in
                     c' >= 65&& c' <= 90 in
    let rec aux acc = function
        [] -> acc
      | [m] when is_upper m -> (Char.escaped m)::acc
      | m::m'::ms when (is_upper m) ->
         let m', tl' =
           if is_upper m' then
             (Char.escaped m), (m'::ms)
           else
             (Char.escaped m) ^ (Char.escaped m'), ms
         in
         aux (m'::acc) tl'
      | _ -> assert false
    in
    List.rev (aux [] (String.explode molecule))

  let starting_molecule = "CRnSiRnCaPTiMgYCaPTiRnFArSiThFArCaSiThSiThPBCaCaSiRnSiRnTiTiMgArPBCaPMgYPTiRnFArFArCaSiRnBPMgArPRnCaPTiRnFArCaSiThCaCaFArPBCaCaPTiTiRnFArCaSiRnSiAlYSiThRnFArArCaSiRnBFArCaCaSiRnSiThCaCaCaFYCaPTiBCaSiThCaSiThPMgArSiRnCaPBFYCaCaFArCaCaCaCaSiThCaSiRnPRnFArPBSiThPRnFArSiRnMgArCaFYFArCaSiRnSiAlArTiTiTiTiTiTiTiRnPMgArPTiTiTiBSiRnSiAlArTiTiRnPMgArCaFYBPBPTiRnSiRnMgArSiThCaFArCaSiThFArPRnFArCaSiRnTiBSiThSiRnSiAlYCaFArPRnFArSiThCaFArCaCaSiThCaCaCaSiRnPRnCaFArFYPMgArCaPBCaPBSiRnFYPBCaFArCaSiAl"

  let count_occurrences molecule =
    let m' = atoms_of_string molecule in
    let rec aux total l1' l2' =
      match (l1', l2') with
        [], _ -> aux (total + 1) m' l2'
      | _, [] -> total
      | hd::tl, hd'::tl' when hd = hd' -> aux total tl tl'
    | _, hd::tl -> aux total m' tl
    in
    aux 0 m' (atoms_of_string starting_molecule)

  let create_new_molecules (f, t) =
    let rec aux acc prev = function
        [] -> acc
      | hd::tl -> if hd = f then
                   let r = List.fold_left (^) "" (prev @ [t] @ tl) in
                   aux (r::acc) (prev @ [hd]) tl
                 else
                   aux acc (prev @ [hd]) tl
    in
    aux [] [] (atoms_of_string starting_molecule)

  let answer_01 = S.cardinal (List.fold_left (fun set s -> S.add s set) S.empty (List.concat (List.map create_new_molecules substitutions)))

  let count_atoms atom  =
    let rec aux total = function
        [] -> total
      | hd::tl when hd = atom -> aux (total + 1) tl
      | _::tl -> aux total tl
    in
    aux 0 (atoms_of_string starting_molecule)

  let ars = count_atoms "Ar"
  let rns = count_atoms "Rn"
  let ys  = count_atoms "Y"

  let answer_02 = List.length (atoms_of_string starting_molecule) - ars - rns - (2 * ys) - 1
end
