(* RPG *)
module RPG = struct
  type weapon = { name: string; cost: int; damage: int }
  type armor  = { name: string; cost: int; armor: int }
  type d_ring = { cost: int; damage: int }
  type a_ring = { cost: int; armor: int }

  type ring = DRing of d_ring | ARing of a_ring
  type loadout = weapon * armor option * ring list
  type character = { name: string; hit_points: int; loadout: loadout }
  type boss      = { hit_points: int; damage: int; armor: int }

  let damage_modifier (i:weapon) = i.damage
  let armor_modifier  (i:armor) = i.armor

  let ring_damage = function DRing r -> r.damage | _ -> 0
  let ring_armor  = function ARing a -> a.armor  | _ -> 0

  let total_ring_damage = List.fold_left (fun total r -> total + ring_damage r) 0
  let total_ring_armor =  List.fold_left (fun total r -> total + ring_armor r) 0

  let ring_cost = function ARing r -> r.cost | DRing r -> r.cost

  let character_damage {loadout = (w, _, rs)}  = damage_modifier w + total_ring_damage rs
  let character_hit_points ({hit_points = hit_points}:character) = hit_points
  let character_armor  {loadout = (_, a, rs)}  = (match a with Some a -> armor_modifier a | _ -> 0) + total_ring_armor rs
  let total_cost   ((w, a, r):loadout): int =
    w.cost +
      (match a with Some a -> a.cost | _ -> 0) +
      (List.fold_left
         (fun total r -> (ring_cost r) + total)
         0 r)

  let character_from_loadout name hp l = { name = name; hit_points = hp; loadout = l}
  let boss = { hit_points = 103;
               armor = 2;
               damage = 9; }

  let weapon (name, cost, damage) = { name = name; cost = cost; damage = damage }
  let armor  (name, cost, armor)  = { name = name; cost = cost; armor  = armor }
  let d_ring (cost, damage) = DRing { cost = cost; damage = damage }
  let a_ring (cost, armor) = ARing { cost = cost; armor = armor }

  let weapons = List.map weapon [("Dagger", 8, 4);
                                 ("Shortsword", 10, 5);
                                 ("Warhammer", 25, 6);
                                 ("Longsword", 40, 7);
                                 ("Greataxe", 74, 8)]
  let armors = List.map armor [("Leather", 13, 1);
                               ("Chainmail", 31, 2);
                               ("Splintmail", 53, 3);
                               ("Bandedmail", 75, 4);
                               ("Platemail", 102, 5)]
  let rings = (List.map d_ring [(25, 1);
                                (50, 2);
                                (100, 3)]) @
                (List.map a_ring [(20, 1);
                                  (40, 2);
                                  (80, 3)])

  let rings = [] @ (* 0 *)
                (List.map (fun r -> [r]) rings) @ (* 1 *)
                  (List.concat
                     (List.map (fun r -> (List.map (fun r' -> [r;r']) rings)) rings)) (* 2 *)

  let characters =
    let loadouts : loadout list =
      let legal_loadout (weapon, armor, rings) =
        match List.length rings with
          0 | 1 -> true
          | 2 -> not (List.hd rings = List.hd (List.tl rings))
          | _ -> false in
      let armors = None::(List.map (fun a -> Some a) armors) in
      List.filter legal_loadout
                  (List.flatten (List.map (fun w -> (List.concat (List.map (fun a -> (List.map (fun rs -> (w, a, rs)) rings)) armors))) weapons))
    in
    List.mapi (fun i l -> character_from_loadout (Printf.sprintf "Me #%04d" i) 100 l) loadouts

  let beat_boss (character:character) =
    let rec aux b c =
      let chp = character_hit_points c in
      let ca  = character_armor c in
      let cd  = character_damage c in
      match (b.hit_points, chp) with
        b', _ when b' <= 0 -> true
      | _, c' when c' <= 0 -> false
      | _, _   -> aux { b with hit_points = (b.hit_points - (max (cd - b.armor) 1)) }
                     { c with hit_points = (chp - (max (b.damage - ca) 1)) }
    in
    aux boss character

  let fights =
    List.sort (fun (_, a) (_, b) -> compare a b)
              (List.map (fun c -> beat_boss c, total_cost c.loadout) characters)

  let answer_01 = List.hd (List.filter (fun (b, _) -> b) fights)
  let answer_02 = List.hd (List.rev (List.filter (fun (b, _) -> not b) fights))
end

let () =
  let cost = function
    | (_, c) -> c in
  print_endline (Printf.sprintf "Answer 01: %d" (cost RPG.answer_01));
  print_endline (Printf.sprintf "Answer 02: %d" (cost RPG.answer_02));
