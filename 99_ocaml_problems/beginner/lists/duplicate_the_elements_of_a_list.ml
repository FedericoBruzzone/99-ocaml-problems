let rec duplicate = function
    | [] -> []
    | h :: t -> h :: h :: duplicate t
;;

(* Tail recursive *)
let duplicate l = 
    let rec aux acc = function
        | [] -> acc
        | h :: t -> aux (h :: h :: acc) t
    in 
    List.rev (aux [] l)
;;

duplicate ["a"; "b"; "c"; "c"; "d"];;
