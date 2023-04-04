let rec fib a =
        match a with
        | 0 | 1 -> 1
        | x when x > 0 -> fib (x - 1) + fib (x - 2) 
        | _ -> raise (Invalid_argument "Negative value supplied to fib") 

let rec fib_range a =
        match fib a with
        | x when x > 4_000_000 -> []
        | _ -> fib a :: fib_range (a + 1)


let rec even_fib l =
        match l with
        | [] -> 0
        | h :: t ->
                        match h mod 2 with
                        | 0 -> h + even_fib t
                        | _ -> even_fib t

let _ = Printf.printf "%i\n" @@ even_fib @@ fib_range 1
