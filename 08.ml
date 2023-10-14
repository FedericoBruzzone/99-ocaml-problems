(*
  8. Eliminate consecutive duplicates of list elements. (medium)
*)

let compress (l : 'a list) : 'a list =
  let rec aux (acc : 'a list) (l : 'a list) =
    match l with
    | [] -> acc
    | [ x ] -> x :: acc
    | f :: (s :: _ as rest) ->
        if f = s then aux acc rest else aux (f :: acc) rest
  in
  aux [] l |> List.rev
;;

compress
  [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
