(*
  38. Compare the two methods of calculating Euler's totient function. (easy)

  Use the solutions of problems "Calculate Euler's totient function φ(m)" and
  "Calculate Euler's totient function φ(m) (improved)" to compare the
  algorithms. Take the number of logical inferences as a measure for
  efficiency. Try to calculate φ(10090) as an example.
*)

let factors n =
  let rec aux d n =
    if n = 1 then []
    else if n mod d = 0 then
      match aux d (n / d) with
      | (h, n) :: t when h = d -> (h, n + 1) :: t
      | l -> (d, 1) :: l
    else aux (d + 1) n
  in
  aux 2 n

let phi_improved n =
  let par n m =
    let fn = float_of_int n in
    let fm = float_of_int m in
    int_of_float ((fn -. 1.) *. Float.pow fn (fm -. 1.))
  in
  let rec aux acc l =
    match l with [] -> acc | h :: t -> aux (acc * par (fst h) (snd h)) t
  in
  aux 1 (factors n)

let gcd n1 n2 =
  let mx, mn = (max n1 n2, min n1 n2) in
  let rec aux f s = if s > 0 then aux s (f mod s) else f in
  aux mx mn

let coprime n1 n2 = gcd n1 n2 = 1
let int_of_bool b = match b with true -> 1 | false -> 0

let phi n =
  let rec aux x acc =
    match x with 0 -> acc | x -> aux (x - 1) (acc + int_of_bool (coprime n x))
  in
  aux n 0

let timeit f n =
  let start = Sys.time () in
  let _ = f n in
  let stop = Sys.time () in
  Printf.printf "Execution time: %fs\n" (stop -. start)
;;

timeit phi 10090;;
timeit phi_improved 10090
