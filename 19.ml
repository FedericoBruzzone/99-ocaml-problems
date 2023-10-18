(*
  19. Rotate a list N places to the left. (medium)
*)

let rotate l n =
  let rec aux l n =
    match l with
    | _ when n = 0 -> l
    | h :: t -> aux (List.rev (h :: List.rev t)) (n - 1)
    | _ -> []
  in
  aux l (List.length l + (n mod List.length l))
;;

rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] 3;;
rotate [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" ] (-2)
