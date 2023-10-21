(*
  23. Extract a given number of randomly selected elements from a list. (medium)

  The selected items shall be returned in a list. We use the Random module
  but do not initialize it with Random.self_init for reproducibility.
*)

let rand_select l n =
  let rec remove_and_get_at l i =
    match l with
    | [] -> ([], "")
    | h :: t ->
        if i = 0 then (t, h)
        else
          let t, e = remove_and_get_at t (i - 1) in
          (h :: t, e)
  in
  let rec aux l n acc =
    match n with
    | 0 -> acc
    | x ->
        let l, e = remove_and_get_at l (Random.int (List.length l)) in
        aux l (x - 1) (e :: acc)
  in
  aux l n []
;;

rand_select [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3
