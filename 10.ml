(*
  10. Run-length encoding of a list. (easy)
*)

let encode l =
  let rec aux acc count = function
    | [] -> []
    | [ x ] -> (count + 1, x) :: acc
    | a :: (b :: _ as rest) ->
        if a = b then aux acc (count + 1) rest
        else aux ((count + 1, a) :: acc) 0 rest
  in
  List.rev (aux [] 0 l)
;;

encode [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
