open Batteries
open Utilities
open Printf

let parse arr y s =
  String.iteri
    (fun x c ->
       match c with
       | '@' -> Matrix.put arr (x, y) 1
       | _ -> ())
    s
;;

let is_reachable m c =
  match Matrix.get m c with
  | 1 ->
    let num_rolls =
      Matrix.surrounding_in_matrix m c |> List.filter (Int.equal 1) |> List.length
    in
    num_rolls < 4
  | _ -> false
;;

let move_all_the_things m =
  let move_thing a c =
    match is_reachable m c with
    | true ->
      Matrix.put m c 0;
      a + 1
    | false -> a
  in
  let rec aux acc =
    match Matrix.fold_left move_thing 0 m with
    | 0 -> acc
    | n -> aux (acc + n)
  in
  aux 0
;;

let part_1 () =
  print_newline ();
  let raw = read_file "inputs/day_04.txt" in
  let width = String.length @@ List.hd raw in
  let height = List.length raw in
  let arr = Matrix.make ~width ~height ~f:(fun _ _ -> 0) in
  List.iteri (parse arr) raw;
  let res = Matrix.count arr is_reachable in
  printf "%d\n" res
;;

let part_2 () =
  let raw = read_file "inputs/day_04.txt" in
  let width = String.length @@ List.hd raw in
  let height = List.length raw in
  let arr = Matrix.make ~width ~height ~f:(fun _ _ -> 0) in
  List.iteri (parse arr) raw;
  let res = move_all_the_things arr in
  printf "%d\n" res
;;
