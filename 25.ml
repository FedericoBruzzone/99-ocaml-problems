(*
  25. Generate a random permutation of the elements of a list. (easy)
*)

let permutation l =
  let rec remove_and_get_at l i =
    match l with
    | [] -> raise Not_found
    | h :: t -> (
        match i with
        | 0 -> (t, h)
        | i ->
            let l, e = remove_and_get_at t (i - 1) in
            (h :: l, e))
  in
  let rec permutation l acc =
    match l with
    | [] -> acc
    | l ->
        let new_l, e = remove_and_get_at l (Random.int (List.length l)) in
        permutation new_l (e :: acc)
  in
  permutation l []
;;

permutation [ "a"; "b"; "c"; "d"; "e"; "f" ]
