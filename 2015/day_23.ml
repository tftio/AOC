(* register machine *)
open Batteries

type offset = int
type register = A | B
type instruction = HLF of register
                 | TPL of register
                 | INC of register
                 | JMP of offset
                 | JIE of register * offset
                 | JIO of register * offset

type computer_state = { a : int;
                        b : int;
                        instruction_pointer: int;
                        stack: instruction list }

let new_state = { a = 0; b = 0; instruction_pointer = 0; stack = [] }

exception IllegalRegister of string
let string_of_register = function A -> "a" | B -> "b"
let register_of_string = function ("a" | "a,") -> A | ("b" | "b,") -> B | s -> raise (IllegalRegister s)

exception IllegalInstruction of string
let instruction_of_string s =
  let offset_of_string s' =
    let mult = match (String.sub s' 0 1) with
        "+" -> 1
      | "-" -> (-1)
      | _ -> raise (IllegalInstruction s')
    in
    let num = int_of_string (String.sub s' 1 ((String.length s') - 1)) in
    num * mult
  in
  match (String.nsplit s " ") with
    ([]|[_]) -> raise (IllegalInstruction ("too short: " ^ s))
  | instr::r::[] -> (match instr with
                      "jmp" -> JMP (offset_of_string r)
                    | "inc" -> INC (register_of_string r)
                    | "tpl" -> TPL (register_of_string r)
                    | "hlf" -> HLF (register_of_string r)
                    | _     -> raise (IllegalInstruction ("instr: " ^ s)))
  | jmp::r::o::[] -> (match jmp with
                       "jio" -> JIO (register_of_string r, offset_of_string o)
                     | "jie" -> JIE (register_of_string r, offset_of_string o)
                     | _     -> raise (IllegalInstruction ("jmp: " ^ s)))
  | _ -> raise (IllegalInstruction s)

let string_of_instruction = function
    HLF r -> "hlf " ^ (string_of_register r)
  | TPL r -> "tpl " ^ (string_of_register r)
  | INC r -> "inc " ^ (string_of_register r)
  | JMP o -> "jmp " ^ (string_of_int o)
  | JIE (r, o) -> "jie " ^ (string_of_register r) ^ ", " ^ (string_of_int o)
  | JIO (r, o) -> "jio " ^ (string_of_register r) ^ ", " ^ (string_of_int o)

let inputs = List.rev (List.map instruction_of_string (BatEnum.fold (fun acc l -> l::acc) [] (File.lines_of "day_23.input")))

let read_register state = function A -> state.a | B -> state.b
let set_register {a; b; stack; instruction_pointer} register value =
    match register with
      A -> { a = value; b; stack; instruction_pointer }
    | B -> { a; b = value; stack; instruction_pointer }
let jump {a;b;stack;instruction_pointer} offset = {a;b;stack;instruction_pointer = instruction_pointer + offset }
let current_instruction {a;b;stack;instruction_pointer} = List.nth stack instruction_pointer
let legal_state {a;b;stack;instruction_pointer} = instruction_pointer >= 0 && instruction_pointer < List.length stack

let execute_one_instruction initial_state =
  let instr = current_instruction initial_state in
  match instr with
    HLF r -> jump (set_register initial_state r ((read_register initial_state r) / 2)) 1
  | TPL r -> jump (set_register initial_state r ((read_register initial_state r) * 3)) 1
  | INC r -> jump (set_register initial_state r ((read_register initial_state r) + 1)) 1
  | JMP o -> jump initial_state o
  | JIE (r, o) -> jump initial_state (if (read_register initial_state r) mod 2 = 0 then o else 1)
  | JIO (r, o) -> jump initial_state (if (read_register initial_state r) = 1 then o else 1)

let input_state = { new_state with stack = inputs }

let execute initial_state =
  let rec aux { a; b; stack; instruction_pointer } i =
    let state' = { a; b; stack; instruction_pointer } in
    if not (legal_state state') then
      state'
    else
      (print_endline (Printf.sprintf "instr #%d -> %s" i (string_of_instruction (current_instruction state')));
       aux (execute_one_instruction state') (i + 1))
  in
  aux initial_state 0

let () =
  let a1 = execute input_state in
  let a2 = execute { input_state with a = 1 } in
  print_endline ("Answer 01: " ^ (string_of_int (read_register a1 B)));
  print_endline ("Answer 02: " ^ (string_of_int (read_register a2 B)))
