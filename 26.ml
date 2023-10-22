(*
  26. Generate the combinations of K distinct objects chosen from the N elements of a list. (medium)

  In how many ways can a committee of 3 be chosen from a group of 12 people?
  We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the
  well-known binomial coefficients). For pure mathematicians, this result may
  be great. But we want to really generate all the possibilities in a list.
*)

let rec extract n l =
  if n = 0 then [ [] ]
  else
    match l with
    | [] -> []
    | h :: t ->
        let f = List.map (fun x -> h :: x) (extract (n - 1) t) in
        let s = extract n t in
        f @ s
;;

extract 2 [ "a"; "b"; "c"; "d" ]
