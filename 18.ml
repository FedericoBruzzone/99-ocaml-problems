(*
  18. Extract a slice from a list. (medium)

  Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 0 (this is the way the List module numbers elements).
*)

let slice l first last =
  let rec aux l i =
    match l with
    | _ when i > last -> []
    | h :: t when i < first -> aux t (i + 1)
    | h :: t -> h :: aux t (i + 1)
    | _ -> []
  in
  aux l 0
;;

slice [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 2 6
