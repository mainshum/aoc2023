open Core
let matcher str = 
  Re.matches (Re.compile @@ Re.rep1 Re.digit) str;;

let (+-) x y = (x +. y, x -. y)

let roots time dist = 
  let delta = (time *. time) -. (4. *. dist) in
  let (nom2, nom1) = (+-) time (sqrt delta)  in
  (nom1 /. 2., nom2 /. 2.);;

let calc_integers_in_between_two_floats (lower, upper) = 
  let lower = if Float.is_integer lower then lower +. 1. else lower in
  let upper = if Float.is_integer upper then upper -. 1. else upper in
  (int_of_float @@ Float.round_down upper) - (int_of_float @@ Float.round_up lower) + 1;;

let solve1 xs =
  xs 
  |> List.map ~f:(fun (t, d) -> roots (float_of_string t) (float_of_string d))
  |> List.map ~f:(calc_integers_in_between_two_floats)
  |> List.fold ~init:1 ~f:( * )

let solve2 times dists = 
  let times_con = String.concat ~sep:"" times in
  let dists_con = String.concat ~sep:"" dists in
  let r = roots (float_of_string times_con) (float_of_string dists_con) in
  calc_integers_in_between_two_floats r

let () =  
  match Core.In_channel.read_lines "input-1.txt" with
    | [timeStr; distStr] -> 
        let times = matcher timeStr in
        let dists = matcher distStr in
        let sol1 = solve1 @@ List.zip_exn times dists in
        let sol2 = solve2 times dists in
        Printf.printf "Sol1: %d\n" sol1;
        Printf.printf "Sol2: %d\n" sol2;
    | _ -> failwith "Invalid input";;