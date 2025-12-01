open Batteries

let read_file fname =
  File.lines_of fname |> List.of_enum
;;
