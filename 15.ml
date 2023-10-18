(*

  15. Replicate the elements of a list a given number of times. (medium)
*)

let replicate l n =
  let rec times n h acc =
    match n with 0 -> acc | n -> times (n - 1) h (h :: acc)
  in
  List.fold_right (times n) l []
  (* let rec aux l acc = *)
  (*   match l with [] -> acc | h :: t -> aux t (times n h acc) *)
  (* in *)
  (* aux l [] |> List.rev *)
;;

replicate [ "a"; "b"; "c" ] 3;;
