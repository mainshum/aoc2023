open Core
let matcher str = 
  Re.matches (Re.compile @@ Re.rep1 Re.digit) str;;

let roots time dist = 
  let delta = (time *. time) -. (4. *. dist) in
  let nom1 = time -. (sqrt delta) in
  let nom2 = time +. (sqrt delta) in
  (nom1 /. 2., nom2 /. 2.);;

let calc_integers_in_between_two_floats (lower, upper) = 
  let lower = if Float.is_integer lower then lower +. 1. else lower in
  let upper = if Float.is_integer upper then upper -. 1. else upper in
  (int_of_float @@ Float.round_down upper) - (int_of_float @@ Float.round_up lower) + 1;;

let () =  
  match Core.In_channel.read_lines "input-1.txt" with
    | [timeStr; distStr] -> 
        let times = matcher timeStr in
        let dists = matcher distStr in
        List.zip_exn times dists 
        |> List.map ~f:(fun (t, d) -> roots (float_of_string t) (float_of_string d))
        |> List.map ~f:(calc_integers_in_between_two_floats)
        |> List.fold ~init:1 ~f:( * )
        |> Printf.printf "%d\n"
    | _ -> failwith "Invalid input";;