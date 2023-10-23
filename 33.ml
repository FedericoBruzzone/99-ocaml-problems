(*
  33. Determine whether two positive integer numbers are coprime. (easy)

  Two numbers are coprime if their greatest common divisor equals 1.
*)

let gcd n1 n2 =
  let mx, mn = (max n1 n2, min n1 n2) in
  let rec aux f s = if s > 0 then aux s (f mod s) else f in
  aux mx mn

let coprime n1 n2 = gcd n1 n2 = 1;;

coprime 13 27;;
not (coprime 20536 7826)
