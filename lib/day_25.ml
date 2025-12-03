open Batteries
open Utilities
open Printf

let part_1 () =
  print_newline ();
  let raw = read_file "examples/day_02.txt" in
  printf "%d\n" (List.length raw)
;;

let part_2 () =
  let raw = read_file "examples/day_02.txt" in
  printf "%d\n" (List.length raw)
;;
