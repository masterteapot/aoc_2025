open Batteries
open Utilities
open Printf

type border =
  { min_x : int
  ; min_y : int
  ; max_x : int
  ; max_y : int
  }

let print_area = function
  | ((lx, ly), (rx, ry)), area ->
    Printf.printf "(%d,%d) * (%d,%d) -> %d\n" lx ly rx ry area
;;

let parse s =
  let l, r = String.split s ~by:"," in
  Int.of_string l, Int.of_string r
;;

let calc_area (ll, lr) (rl, rr) = (Int.abs (ll - rl) + 1) * (Int.abs (lr - rr) + 1)

let calc arr =
  let arr_len = Array.length arr in
  assert (arr_len >= 2);
  let max_area = ref min_int in
  let rec aux curr next =
    if curr >= arr_len then
      ()
    else if next >= arr_len then
      aux (curr + 1) (curr + 2)
    else (
      let () =
        match calc_area arr.(curr) arr.(next) with
        | area when area > !max_area -> max_area := area
        | _ -> ()
      in
      aux curr (next + 1))
  in
  let () = aux 0 1 in
  !max_area
;;

let calc_v2 arr =
  let arr_len = Array.length arr in
  assert (arr_len >= 2);
  let rec aux curr next ls =
    if curr >= arr_len then
      List.sort (fun (_, l) (_, r) -> Int.compare l r * -1) ls
    else if next >= arr_len then
      aux (curr + 1) (curr + 2) ls
    else (
      let cv = arr.(curr) in
      let nv = arr.(next) in
      aux curr (next + 1) (((cv, nv), calc_area cv nv) :: ls))
  in
  aux 0 1 []
;;

let find_borders (lx, ly) (rx, ry) =
  { min_x = Int.min lx rx
  ; min_y = Int.min ly ry
  ; max_x = Int.max lx rx
  ; max_y = Int.max ly ry
  }
;;

let is_between_x b (x, _) = x > b.min_x && x < b.max_x
let is_between_y b (_, y) = y > b.min_y && y < b.max_y
let is_cross_x b line = line.min_x <= b.min_x && line.max_x >= b.max_x
let is_cross_y b line = line.min_y <= b.min_y && line.max_y >= b.max_y

(* TODO: doesn't handle all cases (9, 7) -> (2, 5) *)
let no_intersections arr b =
  let rec aux i acc =
    if acc = false then
      false
    else if i + 1 >= Array.length arr then (
      let next = find_borders arr.(i) arr.(0) in
      (not (is_between_x b arr.(i) && is_between_y b arr.(i)))
      && (not (is_between_x b arr.(i) && is_cross_y b next))
      && not (is_between_y b arr.(i) && is_cross_x b next))
    else (
      let next = find_borders arr.(i) arr.(i + 1) in
      let new_acc =
        (not (is_between_x b arr.(i) && is_between_y b arr.(i)))
        && (not (is_between_x b arr.(i) && is_cross_y b next))
        && not (is_between_y b arr.(i) && is_cross_x b next)
      in
      aux (i + 1) new_acc)
  in
  aux 0 true
;;

let filter_area arr ((l, r), _) =
  let b = find_borders l r in
  no_intersections arr b
;;

let part_1 () =
  print_newline ();
  let arr = read_file "inputs/day_09.txt" |> List.map parse |> Array.of_list in
  printf "%d\n" (calc arr)
;;

let part_2 () =
  let arr = read_file "inputs/day_09.txt" |> List.map parse |> Array.of_list in
  let areas = calc_v2 arr |> List.filter (filter_area arr) in
  printf "%d\n" (snd @@ List.hd areas)
;;
