(*
  6. Find out whether a list is a palindrome. (easy)
*)

let is_palindrome l =
  let lr = List.rev l in
  let rec aux l lr =
    match (l, lr) with
    | [], [] -> true
    | h :: t, hr :: tr -> if h = hr then aux t tr else false
    | _, _ -> false
  in
  aux l lr
;;

is_palindrome [ "x"; "a"; "m"; "a"; "x" ];;
not (is_palindrome [ "a"; "b" ])
