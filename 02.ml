(*
  2. Find the last but one (last and penultimate) elements of a list. (easy)
*)

let rec last_two = function
  | [] -> None
  | [ _ ] -> None
  | [ x; y ] -> Some [ x; y ]
  | _ :: t -> last_two t
;;

last_two [ "a"; "b"; "c"; "d" ];;
last_two [ "a" ]
