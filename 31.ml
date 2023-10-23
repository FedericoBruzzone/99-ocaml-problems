(*
  31. Determine whether a given integer number is prime. (medium)
*)

let is_prime n =
  let rec is_prime' d =
    if d * d > n then true
    else if n mod d = 0 then false
    else is_prime' (d + 1)
  in
  n <= 1 && is_prime' 2
;;

not (is_prime 1);;
is_prime 7;;
not (is_prime 12);;
