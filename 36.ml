(*
  36. Determine the prime factors of a given positive integer (2). (medium)

  Construct a list containing the prime factors and their multiplicity. Hint:
  The problem is similar to problem Run-length encoding of a list
  (direct solution).
*)

let factors n =
  let rec aux acc n d =
    match n with
    | 1 -> acc
    | n ->
        if n mod d = 0 then aux ((d, 1) :: acc) (n / d) d else aux acc n (d + 1)
  in
  aux [] n 2
  |> List.fold_left
       (fun acc (d, c) ->
         match acc with
         | [] -> [ (d, c) ]
         | (d', c') :: t -> if d = d' then (d, c + c') :: t else (d, c) :: acc)
       []
;;

factors 315
(* : (int * int) list = [(3, 2); (5, 1); (7, 1)] *)
