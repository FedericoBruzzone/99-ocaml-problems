(*
  41. A list of Goldbach compositions. (medium)

  Given a range of integers by its lower and upper limit, print a list of all
  even numbers and their Goldbach composition.

  In most cases, if an even number is written as the sum of two prime numbers,
  one of them is very small. Very rarely, the primes are both bigger than say
  50. Try to find out how many such cases there are in the range 2..3000.
*)

let is_prime n =
  if n = 1 then false
  else
    let n = max n (-n) in
    let rec is_not_divisor d =
      d * d > n || (n mod d <> 0 && is_not_divisor (d + 1))
    in
    is_not_divisor 2

let goldbach n =
  let rec aux f t =
    if is_prime f && is_prime t then (f, t) else aux (f + 1) (t - 1)
  in
  aux 0 n

let rec goldbach_list f t =
  if f > t then []
  else if f mod 2 = 1 then goldbach_list (f + 1) t
  else (f, goldbach f) :: goldbach_list (f + 2) t

let goldbach_limit f t threshold =
  List.filter
    (fun (_, (a, b)) -> a > threshold && b > threshold)
    (goldbach_list f t)
;;

goldbach_list 9 20;;
goldbach_limit 1 2000 50
