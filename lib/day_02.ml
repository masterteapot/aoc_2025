open Batteries
open Utilities
open Printf

module Part_1 = struct
  let int_to_double_digits i =
    let int_s = String.of_int i in
    String.to_int (int_s ^ int_s)
  ;;

  let first_valid_base s =
    let s_len = String.length s in
    let base_s =
      if Int.rem s_len 2 = 0 then
        s
      else
        String.of_int @@ Int.pow 10 s_len
    in
    let fh = Int.of_string (first_half_of_string base_s) in
    let sh = Int.of_string (second_half_of_string base_s) in
    if fh >= sh then
      fh
    else
      fh + 1
  ;;

  let last_valid_base s =
    let s_len = String.length s in
    let base_s =
      if Int.rem s_len 2 = 0 then
        s
      else
        String.of_int @@ (Int.pow 10 (s_len - 1) - 1)
    in
    let fh = Int.of_string (first_half_of_string base_s) in
    let sh = Int.of_string (second_half_of_string base_s) in
    if fh <= sh then
      fh
    else
      fh - 1
  ;;

  let parse acc s =
    let ls, rs = String.split ~by:"-" s in
    let end_base = last_valid_base rs in
    let start_base = first_valid_base ls in
    let rec aux inacc n =
      if n > end_base then
        inacc
      else
        aux (int_to_double_digits n :: inacc) (n + 1)
    in
    aux acc start_base
  ;;
end

module Part_2 = struct
  let int_to_repeated_str i n =
    let int_s = String.of_int i in
    let rec aux counter =
      if counter > n then
        ""
      else
        aux (counter + 1) ^ int_s
    in
    aux 1
  ;;

  let int_to_repeated_digits i n = String.to_int @@ int_to_repeated_str i n

  let rec min_reps i l =
    if Int.rem l i = 0 then
      l / i
    else
      min_reps i (l + 1)
  ;;

  let rec max_reps i l =
    assert (l > 0);
    if Int.rem l i = 0 then
      l / i
    else
      max_reps i (l - 1)
  ;;

  let lowest_next_step i =
    let len = int_length i in
    Int.pow 10 (len - 1)
  ;;

  let find_repeated_digits ~s_min ~s_max acc width =
    let s_min_len = String.length s_min in
    let s_max_len = String.length s_max in
    let min_num_to_repeat = Int.pow 10 (width - 1) in
    let min = Int.of_string s_min in
    let max = Int.of_string s_max in
    let rec loop_incr_num acc num_to_repeat rep =
      let num = int_to_repeated_digits num_to_repeat rep in
      if num > max || int_length num_to_repeat > width || int_length num < 2 then
        acc
      else if num < min then
        loop_incr_num acc (num_to_repeat + 1) rep
      else
        loop_incr_num (num :: acc) (num_to_repeat + 1) rep
    in
    let rec loop_over_reps d counter last_counter =
      if counter > last_counter then
        []
      else
        loop_incr_num [] d counter
        @ loop_over_reps (lowest_next_step d) (counter + 1) last_counter
    in
    acc
    @ loop_over_reps
        min_num_to_repeat
        (min_reps width s_min_len)
        (max_reps width s_max_len)
  ;;

  let parse acc s =
    let ls, rs = String.split ~by:"-" s in
    let max_width = String.length rs / 2 in
    let rec run_all_widths inacc width =
      if width > max_width then
        inacc
      else (
        let new_acc = find_repeated_digits ~s_min:ls ~s_max:rs inacc width in
        run_all_widths new_acc (width + 1))
    in
    run_all_widths acc 1
  ;;
end

let part_1 () =
  let input =
    read_file "inputs/day_02.txt"
    |> List.hd
    |> String.split_on_char ','
    |> List.fold_left Part_1.parse []
    |> List.rev
  in
  printf "%d\n" (sum_int_list input)
;;

let part_2 () =
  let input =
    read_file "inputs/day_02.txt"
    |> List.hd
    |> String.split_on_char ','
    |> List.fold_left Part_2.parse []
    |> List.unique
  in
  printf "%d\n" (sum_int_list input)
;;
