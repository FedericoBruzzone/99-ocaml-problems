(*
  16. Drop every N'th element from a list. (medium)
*)

let drop l n =
  let rec aux l i =
    match l with
    | [] -> []
    | h :: t -> if not (i mod n = 0) then h :: aux t (i + 1) else aux t (i + 1)
  in
  aux l 1

let drop l n =
  let rec aux l acc i =
    match l with
    | [] -> acc
    | h :: t ->
        if not (i mod n = 0) then aux t (h :: acc) (i + 1) else aux t acc (i + 1)
  in
  aux l [] 1 |> List.rev
;;

drop [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
