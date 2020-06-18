open Batteries;;

let file_as_lines name = BatEnum.fold (fun acc l -> l::acc) [] (File.lines_of name);;

type speed = int;;
type distance = int;;
type duration = int;;
type wins = int;;
type reindeer = string * (speed * duration) * duration;;
type racing_state = Start | Resting of duration | Running of duration;;
type racing_result = wins * distance * racing_state;;

let reindeer_of_string list =
  let ls = String.nsplit list " " in
  List.nth ls 0,
  (int_of_string (List.nth ls 3),
   int_of_string (List.nth ls 6)),
  int_of_string (List.nth ls 13);;

let reindeers = List.map reindeer_of_string (file_as_lines "day_14.input");;

let move_one_second (reindeer, (wins, distance, state)) =
  match reindeer with
    _, (run_speed, run_duration), rest_duration ->
    let (distance', state') = match state with
        Start -> run_speed, Running 1

      | Resting (i) when i < rest_duration -> distance,             Resting (i + 1)
      | Resting _                          -> distance + run_speed, Running 1

      | Running (i) when run_duration = i -> distance,             Resting 1
      | Running (i)                       -> distance + run_speed, Running (i + 1)
    in
    (reindeer, (wins, distance', state'));;

let race reindeers seconds =
  let update_wins current_state =
    let current_max_distance =
      let distances = List.map (fun (_, (_, d, _)) -> d) current_state in
      List.fold_left (fun acc d -> if d > acc then d else acc) 0 distances
    in
    List.map (fun (r, (w, d, s)) -> if d = current_max_distance then
                                   (r, (w + 1, d, s))
                                 else
                                   (r, (w, d, s)))
             current_state
  in
  let rec aux acc = function
      0 -> acc
    | s -> let current_state = List.map move_one_second acc in
          aux (update_wins current_state) (s - 1)
  in
  aux (List.map (fun r -> (r, (0, 0, Start))) reindeers) seconds;;

let answer_01, answer_02, results =
  let results = race reindeers 2503 in
  let sorter f = List.sort (fun a b -> Pervasives.compare (f b) (f a)) in
  let sort_01 = sorter (function _, (_, d, _) -> d) in
  let sort_02 = sorter (function _, (w, _, _) -> w) in
  List.hd (sort_01 results),
  List.hd (sort_02 results),
  results;;
