(*
  3. Find the K'th element of a list. (easy)
*)

let rec at k = function
  | [] -> None
  | h :: t -> if k = 0 then Some h else at (k - 1) t
;;

at 2 [ "a"; "b"; "c"; "d"; "e" ];;
at 2 [ "a" ]
