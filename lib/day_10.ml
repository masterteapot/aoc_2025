open Batteries
open Utilities
open Printf

type electric =
  | On
  | Off

type circuit =
  { lights : electric list
  ; buttons : int list list
  ; jolts : int list
  }

let init_circuit = { lights = []; buttons = []; jolts = [] }

let rec extract = function
  | [] -> []
  | hd :: tl ->
    let more_cases = extract tl in
    (hd, tl) :: List.map (fun (nhd, ntl) -> nhd, hd :: ntl) more_cases
;;

let rec permutations = function
  | [] -> [ [] ]
  | l ->
    List.concat_map
      (fun (x, rest) -> List.map (fun p -> x :: p) (permutations rest))
      (extract l)
;;

let make_perms n =
  let rec make_desc_list i acc =
    if i >= n then
      acc
    else
      make_desc_list (i + 1) (i :: acc)
  in
  permutations (make_desc_list 0 [])
;;

let print_lights ls =
  let print_light = function
    | On -> print_char '#'
    | Off -> print_char '.'
  in
  print_char '[';
  List.iter print_light ls;
  print_char ']'
;;

let print_buttons buttons =
  let rec print_nums = function
    | [] -> ()
    | [ hd ] -> Printf.printf "%d" hd
    | hd :: tl ->
      Printf.printf "%d," hd;
      print_nums tl
  in
  let print_button ls =
    print_char '(';
    print_nums ls;
    print_string ") "
  in
  List.iter print_button buttons
;;

let print_jolts ls =
  let rec print_jolt = function
    | [] -> ()
    | [ hd ] ->
      print_int hd;
      print_char '}'
    | hd :: tl ->
      Printf.printf "%d," hd;
      print_jolt tl
  in
  print_char '{';
  print_jolt ls
;;

let print_circuit circ =
  print_lights circ.lights;
  print_char ' ';
  print_buttons circ.buttons;
  print_jolts circ.jolts;
  print_endline ""
;;

let parse_lights circ s =
  let s_filter acc = function
    | '.' -> Off :: acc
    | '#' -> On :: acc
    | _ -> acc
  in
  let lights = String.explode s |> List.fold_left s_filter [] |> List.rev in
  { circ with lights }
;;

let parse_buttons circ s =
  let s_filter = function
    | '(' | ')' -> ""
    | ('0' .. '9' | ',') as c -> String.of_char c
    | c -> failwith (Printf.sprintf "parse buttons: unexpected char: %c" c)
  in
  let buttons =
    String.replace_chars s_filter s |> String.split_on_char ',' |> List.map Int.of_string
  in
  { circ with buttons = buttons :: circ.buttons }
;;

let parse_jolts circ s =
  let s_filter = function
    | '{' | '}' -> ""
    | ('0' .. '9' | ',') as c -> String.of_char c
    | c -> failwith (Printf.sprintf "parse jolts: unexpected char: %c" c)
  in
  let jolts =
    String.replace_chars s_filter s |> String.split_on_char ',' |> List.map Int.of_string
  in
  { circ with jolts }
;;

let parse_circuit acc s =
  assert (String.length s >= 1);
  match s.[0] with
  | '[' -> parse_lights acc s
  | '(' -> parse_buttons acc s
  | '{' -> parse_jolts acc s
  | _ -> failwith "parse circuit: unexpected char"
;;

let parse s =
  let raw = String.split_on_char ' ' s in
  let cir = init_circuit in
  let cir = List.fold_left parse_circuit cir raw in
  { cir with buttons = List.rev cir.buttons }
;;

let part_1 () =
  print_newline ();
  let raw = read_file "examples/day_10.txt" |> List.map parse in
  List.iter print_circuit raw;
  let perms = make_perms 7 in
  print_endline "";
  printf "%d\n" (List.length perms)
;;

let part_2 () =
  let raw = read_file "examples/day_10.txt" in
  printf "%d\n" (List.length raw)
;;
