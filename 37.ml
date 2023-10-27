(*
  37. Calculate Euler's totient function φ(m) (improved). (medium)

  See problem "Calculate Euler's totient function φ(m)" for the definition of
  Euler's totient function. If the list of the prime factors of a number m is
  known in the form of the previous problem then the function phi(m) can be
  efficiently calculated as follows: Let [(p1, m1); (p2, m2); (p3, m3); ...]
  be the list of prime factors (and their multiplicities) of a given number m.
  Then φ(m) can be calculated with the following formula:

  φ(m) = (p1 - 1) × p1^(m1 - 1) × (p2 - 1) × p2^(m2 - 1) × (p3 - 1) × p3^(m3 - 1) × ⋯)
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
;;

phi_improved 10;;
phi_improved 13
