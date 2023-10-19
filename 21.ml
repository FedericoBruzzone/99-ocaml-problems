(*
  21. Insert an element at a given position into a list. (easy)

  Start counting list elements with 0. If the position is larger or equal to the length of the list, insert the element at the end. (The behavior is unspecified if the position is negative.)
*)

let rec insert_at s p l =
  match l with
    [] -> [s]
  | h :: t -> 
    if p = 0 then
      s :: h :: t
    else
      h :: insert_at s (p - 1) t 
;;

insert_at "alfa" 1 ["a"; "b"; "c"; "d"];;
insert_at "alfa" 3 ["a"; "b"; "c"; "d"];;
insert_at "alfa" 4 ["a"; "b"; "c"; "d"];;
