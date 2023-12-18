open Core

let match_digits str = 
  Re.matches (Re.compile @@ Re.digit) str

let join_first_and_last_digit = 
  function
  | [] -> failwith "empty list"
  | hd :: [] -> hd ^ hd
  | hd :: last -> hd ^ List.hd_exn @@ List.rev last 

let solve_1 lines = 
  List.map ~f:match_digits lines
  |> List.map ~f:join_first_and_last_digit
  |> List.map ~f:int_of_string  
  |> List.map ~f:(fun x -> printf "%d\n" x; x)
  |> List.fold ~init:0 ~f:(+)

let () =  
  let fileName = Sys.get_argv () 
    |> Array.to_list 
    |> List.tl_exn 
    |> List.hd_exn in
  let lines = Core.In_channel.read_lines fileName in
  let result = solve_1 lines in
  Printf.printf "%d\n" result
