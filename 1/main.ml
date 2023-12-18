open Core

let match_digits str = 
  Re.matches (Re.compile @@ Re.digit) str

let regex = Re.compile @@ Re.alt [
    Re.digit; 
    Re.str "one"; 
    Re.str "two"; 
    Re.str "three"; 
    Re.str "four"; 
    Re.str "five"; 
    Re.str "six"; 
    Re.str "seven"; 
    Re.str "eight"; 
    Re.str "nine"]

let replace_with_number str =
  match str with
  | "one" -> "1"
  | "two" -> "2"
  | "three" -> "3"
  | "four" -> "4"
  | "five" -> "5"
  | "six" -> "6"
  | "seven" -> "7"
  | "eight" -> "8"
  | "nine" -> "9"
  | _ -> str

let match_digits_and_digit_names str = 
  let rec match_rec pos list = 
    let g_opt = Re.exec_opt ~pos:pos regex (String.sub str ~len: ((String.length str)-pos) ~pos: pos) in
    match g_opt with
      | None -> list
      | Some g -> 
        match Re.Group.get_opt g 0 with
        | None -> list
        | Some x -> match_rec (pos + 1) (list @ [replace_with_number x])
  in match_rec 0 []
  
let join_first_and_last_digit = 
  function
  | [] -> failwith "empty list"
  | hd :: [] -> hd ^ hd
  | hd :: last -> hd ^ List.hd_exn @@ List.rev last 

let get_first_and_last_elem = 
  function
  | [] -> failwith "empty list"
  | hd :: [] -> [hd; hd]
  | hd :: last -> [hd; List.hd_exn @@ List.rev last]

let solve_1 lines = 
  List.map ~f:match_digits lines
  |> List.map ~f:join_first_and_last_digit
  |> List.map ~f:int_of_string  
  |> List.fold ~init:0 ~f:(+)

let solve_2 lines = 
  List.map ~f:match_digits_and_digit_names lines
  |> List.map ~f:get_first_and_last_elem
  |> List.map ~f:join_first_and_last_digit
  |> List.map ~f:int_of_string  
  |> List.fold ~init:0 ~f:(+)

let () =  
  let fileName = Sys.get_argv () 
    |> Array.to_list 
    |> List.tl_exn 
    |> List.hd_exn in
  let lines = Core.In_channel.read_lines fileName in
  let result = solve_1 lines in
  let result2 = solve_2 lines in
  Printf.printf "%d\n" result;
  Printf.printf "%d\n" result2;
