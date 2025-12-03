open Batteries
open Utilities
open Printf

module Part_1 = struct
  let cmp la ra =
    match la, ra with
    | [| _; lv |], [| _; rv |] -> Int.compare lv rv
    | _ -> failwith "expecting 2 int arrays of length 2"
  ;;

  let parse s =
    let highest_jolts = [| [| 0; 0 |]; [| 0; 0 |] |] in
    let len = String.length s in
    let rec aux i =
      if i >= len then
        ()
      else (
        let x = [| i; char_num_zeroed @@ s.[i] |] in
        if cmp x highest_jolts.(0) > 0 && i < len - 1 then (
          highest_jolts.(0) <- x;
          highest_jolts.(1) <- [| 0; 0 |])
        else if cmp x highest_jolts.(1) > 0 then
          highest_jolts.(1) <- x
        else
          ();
        aux (i + 1))
    in
    aux 0;
    highest_jolts
  ;;

  let find_joltage arr =
    String.of_int arr.(0).(1) ^ String.of_int arr.(1).(1) |> Int.of_string
  ;;
end

module Part_2 = struct
  (** value with the lowest index returns 1 in case of value matches *)
  let cmp la ra =
    match la, ra with
    | [| li; lv |], [| ri; rv |] when lv = rv -> Int.compare li ri * -1
    | [| _; lv |], [| _; rv |] -> Int.compare lv rv
    | _ -> failwith "expecting 2 int arrays of length 2"
  ;;

  let parse num s =
    let highest_jolts = Array.make num [| 0; 0 |] in
    let len = String.length s in
    let rec aux str_i hj_i =
      if hj_i >= num then
        ()
      else if len - str_i <= num - hj_i - 1 then
        aux (highest_jolts.(hj_i).(0) + 1) (hj_i + 1)
      else (
        let x = [| str_i; char_num_zeroed @@ s.[str_i] |] in
        (match cmp x highest_jolts.(hj_i) with
         | 1 -> highest_jolts.(hj_i) <- x
         | _ -> ());
        aux (str_i + 1) hj_i)
    in
    aux 0 0;
    highest_jolts
  ;;

  let find_joltage arr =
    Int.of_string @@ Array.fold_left (fun a i -> a ^ String.of_int i.(1)) "" arr
  ;;
end

let part_1 () =
  let raw = read_file "inputs/day_03.txt" in
  let input = List.map Part_1.parse raw |> List.map Part_1.find_joltage in
  printf "%d\n" (sum_int_list input)
;;

let part_2 () =
  let raw = read_file "inputs/day_03.txt" in
  let input = List.map (Part_2.parse 12) raw |> List.map Part_2.find_joltage in
  printf "%d\n" (sum_int_list input)
;;
