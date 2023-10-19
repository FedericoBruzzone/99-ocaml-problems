(*
  20. Remove the K'th element from a list. (easy)

  The first element of the list is numbered 0, the second 1,...
*)

let rec remove_at n l =
  match l with
  | [] -> []
  | h :: t when n = 0 -> t
  | h :: t -> h :: remove_at (n - 1) t
;;

remove_at 1 [ "a"; "b"; "c"; "d" ]
