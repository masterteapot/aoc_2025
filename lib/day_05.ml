open Batteries
open Utilities
open Printf

type status =
  | Done of (int * int) option * (int * int) option
  | Cont of (int * int) option * (int * int) option

let first_two_of_two_list = function
  | [ hd; md ] -> hd, md
  | _ -> failwith "List should be of length 2"
;;

let merge_ranges (ll, lh) (rl, rh) =
  if lh < rl - 1 then
    Done (Some (ll, lh), Some (rl, rh))
  else if ll > rh + 1 then
    Cont (Some (rl, rh), Some (ll, lh))
  else if ll >= rl && lh <= rh then
    Done (Some (rl, rh), None)
  else if ll < rl then
    if lh <= rh then
      Done (Some (ll, rh), None)
    else
      Cont (Some (ll, lh), None)
  else
    Cont (Some (rl, lh), None)
;;

let build_set set s =
  let ls, rs = String.split s ~by:"-" in
  let li, ri = Int.of_string ls, Int.of_string rs in
  let rec aux r acc = function
    | [] -> List.rev (r :: acc)
    | hd :: tl ->
      (match merge_ranges r hd with
       | Done (Some l, Some r) -> List.rev (r :: l :: acc) @ tl
       | Done (Some l, None) -> List.rev (l :: acc) @ tl
       | Cont (Some l, Some r) -> aux r (l :: acc) tl
       | Cont (Some l, None) -> aux l acc tl
       | _ -> failwith "unexpected result from merge_ranges")
  in
  aux (li, ri) [] set
;;

let is_mem set i =
  let rec aux = function
    | [] -> false
    | (l, h) :: _ when i >= l && i <= h -> true
    | _ :: tl -> aux tl
  in
  aux set
;;

let count_fresh_ids set acc i =
  match is_mem set i with
  | true -> acc + 1
  | false -> acc
;;

let count_all_fresh acc (lo, hi) = acc + hi - lo + 1

let part_1 () =
  let raw_range, raw_id =
    read_file "inputs/day_05.txt" |> group_on_newline |> first_two_of_two_list
  in
  let pids = List.map Int.of_string raw_id in
  let set = List.fold_left build_set [] raw_range in
  let res = List.fold_left (count_fresh_ids set) 0 pids in
  printf "p1: %d\n" res
;;

let part_2 () =
  let raw_range, _ =
    read_file "inputs/day_05.txt" |> group_on_newline |> first_two_of_two_list
  in
  let set = List.fold_left build_set [] raw_range in
  let res = List.fold_left count_all_fresh 0 set in
  printf "p2: %d\n" res
;;
