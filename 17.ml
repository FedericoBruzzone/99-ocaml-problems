(*
  17. Split a list into two parts; the length of the first part is given. (easy)

  If the length of the first part is longer than the entire list, then the
  first part is the list and the second part is empty.
*)

let rec split l n =
  match l with
  | [] -> ([], [])
  | h :: t ->
      let r = split t (n - 1) in
      if n <= 0 then (fst r, t) else (h :: fst r, snd r)

let split l n =
  let rec aux l n acc =
    match l with
    | [] -> (acc, [])
    | h :: t -> if n = 0 then (acc, h :: t) else aux t (n - 1) (h :: acc)
  in
  aux l n [] |> fun (a, b) -> (List.rev a, b)
;;

split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3;;
split [ "a"; "b"; "c"; "d" ] 5
