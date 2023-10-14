(*
  9. Pack consecutive duplicates of list elements into sublists. (medium)
*)

let pack (l : 'a list) =
    let rec aux (acc : 'a list list) (curr : 'a list) (l : 'a list) =
        match l with
        | [] -> []
        | [ x ] -> (x :: curr) :: acc
        | a :: (b :: _ as rest) ->
            if a = b then
                aux acc (a :: curr) rest
            else
                aux ((a :: curr) :: acc) [] rest
    in
    aux [] [] l |> List.rev
;;

pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;

