let rec range i j = 
  if i > j then [] 
  else i :: range (i+1) j

let rec euler_1 l =
  match l with
  | [] -> 0
  | h :: t ->
    match h mod 3 = 0 || h mod 5 = 0 with
    | true -> h + euler_1 t
    | false -> euler_1 t


let _ = Printf.printf "%i\n" @@ euler_1 @@ range 0 999
