(* Wintermute *)

open Batteries

let shuffle lst =
    let lst' = List.map (fun c -> Random.bits(), c) lst in
    List.map snd (List.sort (fun (a, _) (b, _) -> compare a b) lst')

module Wizarding = struct
  type damage = int
  type mana   = int
  type hp     = int
  type rounds = int
  type spell  = Shield of rounds | Poison of rounds | Recharge of rounds
                | MagicMissile | Drain

  type player = { mana: mana; hp: hp; effects: spell list }
  type boss   = { damage: damage; hp: hp; effects: spell list }
  type result = Player | Boss
  type game_state = { player: player; boss: boss; spells_cast: spell list; result: result option }

  let spells = [Shield(6);
                Poison(6);
                Recharge(5);
                MagicMissile;
                Drain]

  let cost = function
      Shield _ -> 113
    | Poison _ -> 173
    | Recharge _ -> 229
    | MagicMissile -> 53
    | Drain -> 73

  let string_of_spell = function
      MagicMissile -> "Magic Missile"
    | Drain -> "Drain"
    | Poison p -> "Poison(" ^ (string_of_int p) ^ ")"
    | Recharge r -> "Recharge(" ^ (string_of_int r) ^ ")"
    | Shield s -> "Shield(" ^ (string_of_int s) ^ ")"

  let string_of_effects effects =
    String.concat ", " (List.map string_of_spell effects)

  let string_of_spells_cast spells =
    List.fold_left (^) "" (List.map (fun s -> Char.escaped (List.hd (String.explode (string_of_spell s)))) spells)


  let string_of_boss { hp; damage; effects } = "B (D=" ^
                           (string_of_int damage) ^
                             ", H=" ^ (string_of_int hp) ^
                               ", E=" ^ (string_of_effects effects) ^ ")"
  let string_of_player { mana; hp; effects } =
    "P (M=" ^ (string_of_int mana) ^
      ", H=" ^ (string_of_int hp) ^
        ", E=" ^ (string_of_effects effects) ^ ")"

  let string_of_result = function
      None -> ""
    | Some Player -> "Yay! Player wins!"
    | Some Boss   -> "BOO! Boss wins!"

  let string_of_state { boss; player; result; spells_cast } =
    String.concat " " [string_of_boss boss;
                       string_of_player player;
                       string_of_result result;
                       string_of_spells_cast spells_cast]

  let total_cost = List.fold_left (fun t s -> t + (cost s)) 0
  let new_player m h = { mana = m; hp = h; effects = []; }
  let new_boss d h = { damage = d; hp = h; effects = [] }
  let new_state b p = { boss = b; player = p; result = None; spells_cast = [] }

  let player_shielded { effects; hp; mana; } =
    List.fold_left (fun shielded spell -> match spell with Shield _ -> true | _ -> shielded) false effects
  let player_recharging { effects; mana; _ } =
    List.fold_left (fun recharging spell -> match spell with Recharge _ -> true | _ -> recharging) false effects
  let boss_poisoned { damage; effects; _ } =
    List.fold_left (fun poisoned spell -> match spell with Poison _ -> true | _ -> poisoned) false effects

  let round state =
    let age_effects e =
      let rec aux acc = function
          []    -> acc
        | s::ss -> let acc' =
                    match s with
                      Shield i -> if i = 0 then acc else (Shield (i - 1))::acc
                    | Poison i -> if i = 0 then acc else (Poison (i - 1))::acc
                    | Recharge i -> if i = 0 then acc else (Recharge (i - 1))::acc
                    | _ -> acc
                  in
                  aux acc' ss
      in
      aux [] e 
    in

    let apply_player_effects { boss ; player ; result ; spells_cast ; _ } =
      let rec aux acc { mana; hp; effects } = function
          [] -> { mana; hp; effects = acc }
        | s::ss -> aux
                    (s::acc)
                    (match s with Recharge _ -> { hp; mana = mana + 101; effects }
                     | _ -> { mana; hp; effects }) ss
      in
      let new_player = aux [] player (age_effects player.effects) in
      { boss; player = new_player; result; spells_cast }
    in

    let apply_boss_effects { boss; player; result; spells_cast } =
      let rec aux acc { hp; damage; effects} = function
          [] -> { hp; damage; effects = acc}
        | s::ss -> aux (s::acc)
                      (match s with
                         Poison _ -> let new_hp = hp - 3 in { hp = new_hp; effects; damage }
                       | _ -> { hp; effects; damage } ) ss
      in
      let boss' = aux [] boss (age_effects boss.effects) in
      let result' = if boss'.hp <= 0 then Some Player else result in
      { boss = boss'; player; result = result'; spells_cast }
    in

    let cast_spell { boss; player; result; spells_cast } =
      match result with
        Some _ -> { boss; player; result; spells_cast }
      | None   ->
         let new_spell =
             let spells' = List.filter
                             (fun s -> (cost s) <= player.mana &&
                                      match s with
                                        Shield _ when (player_shielded player) -> false
                                      | Poison _ when (boss_poisoned boss) -> false
                                      | Recharge _ when (player_recharging player) -> false
                                      | _ -> true) spells in
             if List.length spells' > 0 then
               let new_spell = List.hd (shuffle spells') in
               Some new_spell
             else
               None
         in match new_spell with
              None       -> { boss; player; result; spells_cast }
            | Some spell -> let player_mana = player.mana - (cost spell) in
                           let boss', player' = match spell with
                               Drain -> let boss_hp = boss.hp - 2 in
                                       let player_hp = player.hp + 2 in
                                       { boss with hp = boss_hp }, { player with hp = player_hp }
                             | MagicMissile -> let boss_hp = boss.hp - 4 in
                                              { boss with hp = boss_hp }, player
                             | Shield _ -> boss, { player with effects = spell::player.effects }
                             | Recharge _ -> boss, { player with effects = spell::player.effects; mana = player_mana + 101 }
                             | Poison _ -> let boss_hp = boss.hp - 3 in
                                          { boss with effects = spell::boss.effects; hp = boss_hp }, player
                           in
                           let player'' = { player' with mana = player_mana } in
                           { boss = boss';
                             player = player'';
                             result = if boss'.hp <= 0 then Some Player else result;
                             spells_cast = (spell::spells_cast) }
    in

    let boss_attack { boss; player; result; spells_cast } =
      match result with
        Some _ -> { boss; player; result; spells_cast }
      | None -> let armor = if player_shielded player then 7 else 0 in
               let boss_attack = max 1 (boss.damage - armor) in
               let player_dead = boss_attack >= player.hp in
               let result      = if player_dead then Some Boss else result in
               let hp = max (player.hp - boss_attack) 0 in
               let mana = player.mana in
               let effects = player.effects in
               { boss; player = { hp = hp; mana = mana; effects = effects }; result; spells_cast }
    in
    let round_state = boss_attack (cast_spell (apply_boss_effects (apply_player_effects state))) in
    (print_endline ("\tend of round: " ^ (string_of_state round_state)));
    round_state

  let play boss player =
    let rec aux i state =
      match state.result with
        Some _ -> (print_endline ("***** WE HAVE A WINNER: " ^ (string_of_state state))); state
      | None -> (print_endline ("***** Starting round " ^ (string_of_int i) ^ ": ******"));
               aux (i + 1) (round state)
    in
    aux 1 (new_state boss player)

  let multi_play boss player count =
    let rec aux acc = function
        0 -> acc
      | i -> aux ((play boss player)::acc) (i - 1)
    in
    aux [] count

  let answer_01 iter =
    let wintermute = new_boss 9 58 in
    let neuromancer = new_player 500 50 in
    let all_results = (List.sort (fun (a, _) (b, _) -> compare a b)
                                 (List.filter (fun (c, r) -> match r.result with Some Player -> true | _ -> false)
                                              (List.map (fun r -> (total_cost r.spells_cast), r) (multi_play wintermute neuromancer iter))))
    in
    List.length all_results,
    List.hd all_results
end

let () =
  let iter = 1000000 in
  let (winning_game_count,
       (mana_score,
        winning_state)) = Wizarding.answer_01 iter in
  print_endline (Printf.sprintf "Answer 01: %d (after %d player wins - %.02f%%) =\n%s"
                                mana_score
                                winning_game_count
                                (100. *. (float_of_int winning_game_count) /. (float_of_int iter))
                                (Wizarding.string_of_state winning_state));

