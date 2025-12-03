open Batteries
open Printf

(** Returns the first half of a string. In the case of an odd length string it will return half rounded down.*)
let first_half_of_string s =
  let len = String.length s in
  String.rchop ~n:(len / 2) s
;;

(** Returns the second half of a string. In the case of an odd length string it will return half rounded down.*)
let second_half_of_string s =
  let len = String.length s in
  String.lchop ~n:(len / 2) s
;;

let read_file fname = File.lines_of fname |> List.of_enum

let print_list pf ls =
  print_string "[";
  List.iter (printf pf) ls;
  print_endline "]"
;;

let int_length i = String.length @@ String.of_int i
let sum_int_list ls = List.fold_left ( + ) 0 ls

let group_on_newline ls =
  let rec aux iacc acc = function
    | [] -> List.rev (iacc :: acc)
    | "" :: tl -> aux [] (iacc :: acc) tl
    | hd :: tl -> aux (hd :: iacc) acc tl
  in
  aux [] [] ls
;;

let char_lower_zeroed = function
  | 'a' .. 'z' as c -> Char.code c - Char.code 'a'
  | _ -> failwith "invalid char"
;;

let char_lower_oned c = char_lower_zeroed c + 1

let char_upper_zeroed = function
  | 'A' .. 'Z' as c -> Char.code c - Char.code 'A'
  | _ -> failwith "invalid char"
;;

let char_upper_oned c = char_upper_zeroed c + 1

let char_num_zeroed = function
  | '0' .. '9' as c -> Char.code c - Char.code '0'
  | _ -> failwith "invalid char"
;;

let char_num_oned c = char_num_zeroed c + 1

let rec string_of_list ~sep = function
  | [] -> ""
  | [ s ] -> s
  | s :: tl -> string_of_list ~sep tl ^ sep ^ s
;;

let print_num_array arr =
  print_string "[| ";
  Array.iter (printf "%d; ") arr;
  print_string "|]";
  print_newline ()
;;
