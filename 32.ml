(*
  32. Determine the greatest common divisor of two positive integer numbers. (medium)

  Use Euclid's algorithm.
*)

let gcd n1 n2 =
  let mx, mn = (max n1 n2, min n1 n2) in
  let rec aux f s = if s > 0 then aux s (f mod s) else f in
  aux mx mn
;;

gcd 13 27;;
gcd 20536 7826
