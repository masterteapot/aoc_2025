open Batteries
open Utilities
open Printf
module Ht = Hashtbl

let parse s =
  String.split_on_char ',' s
  |> List.map Int.of_string
  |> function
  | [ x; y; z ] -> [| Float.of_int x; Float.of_int y; Float.of_int z |]
  | _ -> failwith "expected 3 ints"
;;

let square n = Float.pow n 2.0

let distance l r =
  Float.sqrt
  @@ (square (l.(0) -. r.(0)) +. square (l.(1) -. r.(1)) +. square (l.(2) -. r.(2)))
;;

let build_distances ht acc v =
  List.iter (fun x -> Ht.add ht (distance v x) (v, x)) acc;
  v :: acc
;;

let inner_group to_add ls =
  let l, r =
    match to_add with
    | _, (l, r) -> l, r
  in
  let rec aux inacc acc = function
    | [] -> List.unique (List.flatten ([ l; r ] :: inacc)) :: acc
    | hd :: tl when List.mem l hd || List.mem r hd -> aux (hd :: inacc) acc tl
    | hd :: tl -> aux inacc (hd :: acc) tl
  in
  aux [] [] ls
;;

let group max acc count v =
  if count >= max then
    acc
  else
    inner_group v acc
;;

let calc ls =
  (List.length @@ List.at ls 0)
  * (List.length @@ List.at ls 1)
  * (List.length @@ List.at ls 2)
;;

let group_v2 (ls, la) to_add =
  let l, r =
    match to_add with
    | _, (l, r) -> l, r
  in
  let last_added = ref 0.0 in
  last_added := la;
  let rec aux inacc acc = function
    | [] ->
      if List.length inacc > 0 then
        last_added := l.(0) *. r.(0)
      else
        ();
      List.unique (List.flatten ([ l; r ] :: inacc)) :: acc
    | hd :: tl when List.mem l hd || List.mem r hd -> aux (hd :: inacc) acc tl
    | hd :: tl -> aux inacc (hd :: acc) tl
  in
  let new_acc = aux [] [] ls in
  new_acc, !last_added
;;

let part_1 () =
  let input = read_file "inputs/day_08.txt" |> List.map parse in
  let ht = Ht.create 1_000_000 in
  let _ = List.fold_left (build_distances ht) [] input in
  let ls = Ht.to_list ht in
  let ls_sorted = List.sort (fun (l, _) (r, _) -> Float.compare l r) ls in
  let grouped = List.fold_lefti (group 1000) [] ls_sorted in
  let group_sorted =
    List.sort (fun l r -> Int.compare (List.length l) (List.length r) * -1) grouped
  in
  printf "%d\n" (calc group_sorted)
;;

let part_2 () =
  let input = read_file "examples/day_08.txt" |> List.map parse in
  let ht = Ht.create 1_000_000 in
  let _ = List.fold_left (build_distances ht) [] input in
  let ls = Ht.to_list ht in
  let ls_sorted = List.sort (fun (l, _) (r, _) -> Float.compare l r * -1) ls in
  let _, mins = List.fold_left group_v2 ([], 0.0) ls_sorted in
  printf "%.0f\n" mins
;;
