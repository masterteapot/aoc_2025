open Batteries
open Utilities
open Printf

type quantum =
  | Open
  | Cutter
  | Start
  | Beam of int

let run_manifold counter arr =
  let put_beam c =
    match Matrix.get_opt arr c with
    | Some '.' -> Matrix.put arr c '|'
    | _ -> ()
  in
  Matrix.iteri arr (fun c -> function
    | 'S' -> put_beam (Matrix.down c)
    | '^' when Matrix.get arr (Matrix.up c) = '|' ->
      counter := !counter + 1;
      put_beam (Matrix.left c);
      put_beam (Matrix.right c)
    | '.' when Matrix.get_opt arr (Matrix.up c) = Some '|' -> put_beam c
    | _ -> ())
;;

let init_manifold raw =
  let arr =
    Matrix.make
      ~width:(List.length (List.hd raw))
      ~height:(List.length raw)
      ~f:(fun _ _ -> Open)
  in
  List.iteri
    (fun y row ->
       List.iteri
         (fun x c ->
            Matrix.put arr (x, y)
            @@
            match c with
            | 'S' -> Start
            | '.' -> Open
            | '^' -> Cutter
            | _ -> Open)
         row)
    raw;
  arr
;;

let run_manifold_v2 counter arr =
  let put_beam c n =
    match Matrix.get_opt arr c with
    | Some Open -> Matrix.put arr c (Beam n)
    | Some (Beam x) -> Matrix.put arr c (Beam (x + n))
    | _ -> ()
  in
  let timeline_split c =
    match Matrix.get_opt arr (Matrix.left c), Matrix.get_opt arr (Matrix.right c) with
    | Some Open, Some Open
    | Some (Beam _), Some Open
    | Some Open, Some (Beam _)
    | Some (Beam _), Some (Beam _) -> 1
    | _ -> 0
  in
  Matrix.iteri arr (fun c -> function
    | Beam _ -> ()
    | Start -> put_beam (Matrix.down c) 1
    | Cutter ->
      (match Matrix.get_opt arr (Matrix.up c) with
       | Some (Beam n) ->
         counter := !counter + (n * timeline_split c);
         put_beam (Matrix.left c) n;
         put_beam (Matrix.right c) n
       | _ -> ())
    | Open ->
      (match Matrix.get_opt arr (Matrix.up c) with
       | Some (Beam n) -> put_beam c n
       | _ -> ()))
;;

let part_1 () =
  let input =
    read_file "inputs/day_07.txt"
    |> List.map String.explode
    |> List.map Array.of_list
    |> Array.of_list
  in
  let counter = ref 0 in
  run_manifold counter input;
  printf "%d\n" !counter
;;

let part_2 () =
  let input =
    read_file "examples/day_07.txt" |> List.map String.explode |> init_manifold
  in
  let counter = ref 1 in
  run_manifold_v2 counter input;
  printf "%d\n" !counter
;;
