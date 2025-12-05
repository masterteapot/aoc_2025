open Batteries
open Printf

module Matrix = struct
  let make ~width ~height ~f =
    Array.init height (fun y -> Array.init width (fun x -> f x y))
  ;;

  let get m (x, y) = m.(y).(x)

  let in_matrix m (x, y) =
    match m.(y).(x) with
    | exception _ -> false
    | _ -> true
  ;;

  let get_opt m (x, y) =
    match in_matrix m (x, y) with
    | true -> Some m.(y).(x)
    | _ -> None
  ;;

  let in_matrix_filter m a c =
    match get_opt m c with
    | Some v -> v :: a
    | _ -> a
  ;;

  let is_valid m f c = f m c
  let iteri m f = Array.iteri (fun y row -> Array.iteri (fun x _ -> f (x, y)) row) m

  let fold_left f a m =
    Array.fold_lefti
      (fun yacc y row -> Array.fold_lefti (fun xacc x _ -> f xacc (x, y)) yacc row)
      a
      m
  ;;

  let count m f =
    Array.fold_lefti
      (fun acc y row ->
         Array.fold_lefti
           (fun inacc x _ ->
              if is_valid m f (x, y) then
                inacc + 1
              else
                inacc)
           acc
           row)
      0
      m
  ;;

  let put m (x, y) v = m.(y).(x) <- v
  let up (x, y) = x, y - 1
  let right (x, y) = x + 1, y
  let down (x, y) = x, y + 1
  let left (x, y) = x - 1, y
  let up_right (x, y) = x + 1, y - 1
  let down_right (x, y) = x + 1, y + 1
  let down_left (x, y) = x - 1, y + 1
  let up_left (x, y) = x - 1, y - 1
  let up_v m c = up c |> get m
  let right_v m c = right c |> get m
  let down_v m c = down c |> get m
  let left_v m c = left c |> get m
  let up_right_v m c = up_right c |> get m
  let down_right_v m c = down_right c |> get m
  let down_left_v m c = down_left c |> get m
  let up_left_v m c = up_left c |> get m
  let corners c = [ up_right c; down_right c; down_left c; up_left c ]
  let corners_v m c = [ up_right_v m c; down_right_v m c; down_left_v m c; up_left_v m c ]
  let sides c = [ up c; right c; down c; left c ]
  let sides_v m c = [ up_v m c; right_v m c; down_v m c; left_v m c ]
  let corners_in_matrix m c = corners c |> List.fold_left (in_matrix_filter m) []
  let sides_in_matrix m c = sides c |> List.fold_left (in_matrix_filter m) []
  let surrounding c = corners c @ sides c
  let surrounding_v m c = corners_v m c @ sides_v m c

  let surrounding_in_matrix m c =
    corners c @ sides c |> List.fold_left (in_matrix_filter m) []
  ;;

  let print m s =
    Array.iter
      (fun r ->
         Array.iter (printf s) r;
         print_newline ())
      m
  ;;
end

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
    | [] -> List.rev (List.rev iacc :: acc)
    | "" :: tl -> aux [] (List.rev iacc :: acc) tl
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
