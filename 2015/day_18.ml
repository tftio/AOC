open Batteries;;

module Grid = Map.Make(struct
                        type t = int * int
                        let compare (x, y) (x', y') =
                          match Pervasives.compare x x' with
                            0 -> Pervasives.compare y y'
                          | c -> c
                      end)

type light = On | Off

let light_of_char = function
    '.' -> Off
  | '#' -> On
  | _   -> assert false

let char_of_light = function
    On -> '#'
  | Off -> '.'

let cartesian (x, y) (x', y') =
  let range f t =
    let rec aux acc = function
        i when i = t -> List.rev acc
      | i -> aux (i::acc) (i + 1)
    in
    aux [] f
  in
  List.concat (List.map (fun e -> (List.map (fun e' -> (e, e')) (range y y'))) (range x x'))

let on_off grid =
  Grid.fold (fun (x, y) state (on, off) -> match state with On -> (on + 1, off) | Off -> (on, off + 1)) grid (0, 0)

let size = 100, 100
let points = cartesian (0, 0)

let grid_from_list ls =
  let rec aux grid = function
      [] -> grid
    | ((x, y), value)::tl -> aux (Grid.add (x, y) value grid) tl
  in
  aux Grid.empty ls

let grid_from_lines lines =
  let lines' = List.map String.explode lines in
  let x = List.length (List.hd lines') in
  let y = List.length lines in
  let p = points (x, y) in
  grid_from_list (List.map2 (fun a b -> (a, (light_of_char b))) p (List.concat lines'))

let grid_from_file name =
  let lines = BatEnum.fold (fun acc l -> l::acc) [] (File.lines_of name) in
  grid_from_lines lines

let find (x, y) grid =
  if ((x = 0 || x = 99) && (y = 0 || y = 99)) then
    On
  else
    try
      Grid.find (x, y) grid
    with Not_found -> Off

let neighbors grid x y =
  List.remove_assoc (x, y) (List.map (fun (x, y) -> let state = find (x, y) grid
                                                 in
                                                 (x, y), state) (cartesian (x - 1, y - 1) (x + 2, y + 2)))

let calculate_new_state grid x y =
  if ((x = 0 || x = 99) && (y = 0 || y = 99)) then
    On
  else
    let ns = neighbors grid x y in
    let on = List.fold_left (fun total (_, state) -> total + (match state with On -> 1 | _ -> 0)) 0 ns in
    let current_state = find (x, y) grid in
    match on, current_state with
      3, _  -> On
    | 2, On -> On
    | _, _  -> Off

let list_of_grid grid = Grid.fold (fun (x, y) state acc -> ((x, y), state)::acc) grid []

let move grid =
  let rec aux grid' = function
      [] -> grid'
    | ((x,y),_)::tl -> let new_state = calculate_new_state grid x y in
                      aux (Grid.add (x, y) new_state grid') tl
  in
  aux Grid.empty (list_of_grid grid)

let rec moves grid = function
    0 -> grid
  | i -> moves (move grid) (i - 1)

let () =
  let start_grid =
    grid_from_file "/Users/james.black/Dropbox/Projects/advent-of-code/day_18.input" in
  let (on, off) = on_off (moves start_grid 100) in

  print_endline ("100 moves = " ^ (string_of_int on))
