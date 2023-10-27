(*
  35. Determine the prime factors of a given positive integer. (medium)

  Construct a flat list containing the prime factors in ascending order.
*)

let factors n =
  let rec aux acc n d =
    match n with
    | 1 -> acc
    | n -> if n mod d = 0 then aux (d :: acc) (n / d) d else aux acc n (d + 1)
  in
  aux [] n 2 |> List.rev
;;

factors 315
