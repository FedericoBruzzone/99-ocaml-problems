(*
  7. Flatten a nested list structure. (medium)
*)

type 'a node = One of 'a | Many of 'a node list

let flatten (l : 'a node list) =
  let rec aux (acc : 'a list) (l : 'a node list) =
    match l with
    | [] -> acc
    | One x :: rest -> aux (x :: acc) rest
    | Many x :: rest -> aux (aux acc x) rest
  in
  aux [] l |> List.rev
;;

flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
