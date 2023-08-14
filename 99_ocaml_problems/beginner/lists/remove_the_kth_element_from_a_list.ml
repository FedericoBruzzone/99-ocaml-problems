let remove_at count l =
    let rec aux count acc = function
        | [] -> []
        | h :: t -> if count = 0 then acc @ t 
                    else aux (count - 1) (h :: acc) t
        in 
        aux count [] l
;;

let rec remove_at n = function
    | [] -> []
    | h :: t -> if n = 0 then t     
                else h :: remove_at (n - 1) t
;;

remove_at 1 ["a"; "b"; "c"; "d"];;
