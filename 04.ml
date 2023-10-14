(*
  4. Find the number of elements of a list. (easy)
*)

let length l =
  let rec aux n = function [] -> n | _ :: t -> aux (n + 1) t in
  aux 0 l
;;

length [ "a"; "b"; "c" ];;
length []
