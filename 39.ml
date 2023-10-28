(*
  39. A list of prime numbers. (easy)

  Given a range of integers by its lower and upper limit, construct a list of
  all prime numbers in that range.
*)

let is_prime n =
  let n = max n (-n) in
  let rec is_not_divisor d =
    d * d > n || (n mod d <> 0 && is_not_divisor (d + 1))
  in
  is_not_divisor 2

let rec all_primes f t =
  if f > t then []
  else
    let r = all_primes (f + 1) t in
    if is_prime f then f :: r else r
;;

List.length (all_primes 2 7920)
