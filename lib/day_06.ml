open Batteries
open Utilities
open Printf

type calculation =
  | Multiply
  | Add

module P1 = struct
  let parse_nums s =
    let len = String.length s in
    let rec aux acc sacc i =
      if i >= len then (
        match sacc with
        | "" -> List.rev acc
        | s -> List.rev (Int.of_string s :: acc))
      else (
        match s.[i] with
        | ' ' when String.length sacc > 0 -> aux (Int.of_string sacc :: acc) "" (i + 1)
        | ' ' -> aux acc sacc (i + 1)
        | '0' .. '9' as c -> aux acc (sacc ^ String.of_char c) (i + 1)
        | _ -> failwith "unexpected char")
    in
    aux [] "" 0
  ;;

  let parse_calcs s =
    let len = String.length s in
    let rec aux acc i =
      if i >= len then
        acc
      else (
        match s.[i] with
        | ' ' -> aux acc (i + 1)
        | '+' -> aux (Add :: acc) (i + 1)
        | '*' -> aux (Multiply :: acc) (i + 1)
        | _ -> failwith "unexpected char")
    in
    aux [] 0
  ;;

  let parse_input ls =
    let rec aux acc = function
      | [] -> failwith "should not ever have an empty list"
      | [ hd ] -> List.rev acc, parse_calcs hd
      | hd :: tl -> aux (parse_nums hd :: acc) tl
    in
    aux [] ls
  ;;

  let run_calcs arr acc i calc =
    let f, a =
      match calc with
      | Add -> ( + ), 0
      | Multiply -> ( * ), 1
    in
    acc + Array.fold_left (fun a row -> f a row.(i)) a arr
  ;;
end

module P2 = struct
  let cephalopod_num i a s =
    a
    ^
    match s.[i] with
    | '0' .. '9' as c -> String.of_char c
    | _ -> ""
  ;;

  let parse_nums_v2 ls =
    let rec aux inacc acc counter =
      if counter = -1 then (
        match inacc with
        | [] -> List.rev acc
        | ls -> List.rev (List.rev ls :: acc))
      else (
        match List.fold_left (cephalopod_num counter) "" ls with
        | "" -> aux [] (List.rev inacc :: acc) (counter - 1)
        | s -> aux (Int.of_string s :: inacc) acc (counter - 1))
    in
    aux [] [] (String.length (List.hd ls) - 1)
  ;;

  let parse_input_v2 ls =
    let rec aux acc = function
      | [] -> failwith "should not ever have an empty list"
      | [ hd ] -> List.rev acc, P1.parse_calcs hd
      | hd :: tl -> aux (hd :: acc) tl
    in
    let sls, calcs = aux [] ls in
    parse_nums_v2 sls, calcs
  ;;

  let run_calcs_v2 acc nums calc =
    let f, a =
      match calc with
      | Add -> ( + ), 0
      | Multiply -> ( * ), 1
    in
    acc + List.fold_left (fun acc i -> f acc i) a nums
  ;;
end

let part_1 () =
  let open P1 in
  let num_ls, calc_ls = read_file "inputs/day_06.txt" |> parse_input in
  let nums = List.map Array.of_list num_ls |> Array.of_list in
  let calcs = Array.of_list (List.rev calc_ls) in
  assert (Array.length nums.(0) = Array.length calcs);
  let res = Array.fold_lefti (run_calcs nums) 0 calcs in
  printf "%d\n" res
;;

let part_2 () =
  let open P2 in
  let nums, calcs = read_file "inputs/day_06.txt" |> parse_input_v2 in
  assert (List.length nums = List.length calcs);
  let res = List.fold_left2 run_calcs_v2 0 nums calcs in
  printf "%d\n" res
;;
