(*
  34. Calculate Euler's totient function φ(m). (medium)

  Euler's so-called totient function φ(m) is defined as the number of positive
  integers r (1 ≤ r < m) that are coprime to m. We let φ(1) = 1.

  Find out what the value of φ(m) is if m is a prime number. Euler's totient
  function plays an important role in one of the most widely used public key
  cryptography methods (RSA). In this exercise you should use the most
  primitive method to calculate this function (there are smarter ways that
  we shall discuss later).
*)

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
;;

phi 10;;
phi 13
