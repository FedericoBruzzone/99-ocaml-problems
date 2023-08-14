let rev l =
    let rec aux acc = function
        | [] -> acc
        | h :: t -> aux (h :: acc) t
    in
    aux [] l
;;

rev ["a"; "b"; "c"];;
