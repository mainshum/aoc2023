

let group_and_count_chars chars = 
  let rec loop (chars: char list) dict = 
    match chars with
    | [] -> dict 
    | key :: tl -> match List.assoc_opt key dict with
      | Some value -> value.contents <- value.contents + 1; loop tl dict
      | None -> loop tl ((key, ref 1) :: dict)
  in loop chars []


type poker = 
  | Five 
  | Four 
  | House
  | Three 
  | TwoPair 
  | Pair 
  | HighCard 

let poker_map = function
  | Five -> 5
  | Four -> 4
  | House -> 3
  | Three -> 2
  | TwoPair -> 1
  | Pair -> 0
  | HighCard -> -1

let get_type dict = 
  let sorted = List.sort (fun (_, a) (_, b) -> compare b a) dict in
  let open Core in 
  match sorted with
  | [] -> failwith "Invalid input"
  | (_, value) :: tl -> match value.contents with
    | 5 ->  (Five)
    | 4 ->  (Four )
    | 3 -> 
      let (_, value) = List.hd_exn tl in
      if value.contents = 2 then  (House ) else (Three)
    | 2 -> 
        let (_, value) = List.hd_exn tl in
        if value.contents = 2 then  (TwoPair ) else (Pair)
    | 1 ->  (HighCard)
    | _ -> failwith "Invalid input"

let split_string str = 
  str |> Core.String.to_array |> Array.to_list

type hand = {
  hand: string;
  bid: int;
}

let check_str s = 
  try (int_of_string s |> string_of_int) = s
  with Failure _ -> false

let is_some = function
  | Some _ -> true
  | None -> false 

let char_str = function
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> 11
  | 'T' -> 10
  | number -> 
      let open Core in 
      try (String.of_char number |> Int.of_string) with Failure _ -> failwith "Invalid input"

let compare_chars a b = char_str b - char_str a

let compare_hands a b = 
  let x = split_string a.hand in
  let y = split_string b.hand in
  let w_a = x 
  |> group_and_count_chars
  |> get_type
  |> poker_map in
  let w_b = y 
  |> group_and_count_chars
  |> get_type
  |> poker_map in
  print_endline (Printf.sprintf "%s %s %d %d" a.hand b.hand w_a w_b);
  if w_a > w_b then -1 else if w_b > w_a then +1 else 
    let open Core in 
    let a_split = Array.of_list x in
    let b_split = Array.of_list y in
    let rec find_first_higher_char ind = 
      let a_char = Array.get a_split ind in
      let b_char = Array.get b_split ind in
      let compared = compare_chars a_char b_char in
      if compared = 0 then find_first_higher_char(ind +1) else compared
  in find_first_higher_char 0

let x =  
  let open Core in
  let fileName = Sys.get_argv () 
    |> Array.to_list 
    |> List.tl_exn 
    |> List.hd_exn in
  Core.In_channel.read_lines fileName 
  |> List.map ~f:(fun l -> match (String.split_on_chars ~on:[' '] l) with
    | [hand; bid] -> {hand ; bid = Int.of_string bid }
    | _ -> failwith "Invalid input")
  |> List.sort ~compare:(compare_hands)
  |> List.rev
  |> List.foldi ~init:0 ~f:(fun rank acc l -> acc + l.bid * (rank + 1))
  |> Printf.printf "%d\n"