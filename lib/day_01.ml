open Batteries
open Utilities
open Printf

let parse s =
  let dir = s.[0] in
  let num = String.lchop s |> String.to_int in
  match dir with
  | 'L' -> num * -1
  | 'R' -> num
  | _ -> failwith "invalid direction char"
;;

let calc (loc, counter) num =
  let new_loc =
    match Int.rem (loc + num) 100 with
    | 0 -> 0
    | x when x < 0 -> 100 + x
    | x -> x
  in
  match new_loc with
  | 0 -> 0, counter + 1
  | new_curr -> new_curr, counter
;;

let calc_v2 (loc, counter) num =
  let m = Int.abs ((loc + num) / 100) in
  let new_loc = Int.rem (loc + num) 100 in
  match new_loc with
  | 0 when num <= 0 -> 0, counter + 1 + m
  | nl when nl < 0 && loc > 0 -> 100 + nl, counter + 1 + m
  | nl when nl < 0 -> 100 + nl, counter + m
  | nl -> nl, counter + m
;;

let part_1 () =
  print_newline ();
  let raw = read_file "inputs/day_01.txt" in
  let input = List.map parse raw in
  let _, res = List.fold_left calc (50, 0) input in
  printf "%d\n" res
;;

let part_2 () =
  let raw = read_file "inputs/day_01.txt" in
  let input = List.map parse raw in
  let _, res = List.fold_left calc_v2 (50, 0) input in
  printf "%d\n" res
;;
