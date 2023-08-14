let rec length = function
    | [] -> 0
    | _ :: t -> 1 + length t
;;

(* Tail recursive *)
let length l = 
    let rec aux n = function
        | [] -> n 
        | _ :: t -> aux (n + 1) t
    in 
    aux 0 l
;;

length ["a"; "b"; "c"];;
length [];;
