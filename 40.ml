(*
  40. Goldbach's conjecture. (medium)

  Goldbach's conjecture says that every positive even number greater than 2 is
  the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most
  famous facts in number theory that has not been proved to be correct in the
  general case. It has been numerically confirmed up to very large numbers.
  Write a function to find the two prime numbers that sum up to a given even
  integer.
*)

let is_prime n =
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
;;

goldbach 28
