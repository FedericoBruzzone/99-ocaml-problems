let permutation l =
    let rec get_element l index acc = match l with
        | [] -> raise Not_found
        | h :: t -> if index == 0 then
                        h, (acc @ t)
                    else
                        get_element t (index - 1) (h :: acc)
    in
    let get_random_element l length = 
        get_element l (Random.int length) [] 
    in
    let rec aux l length acc = 
        if length == 0 then acc
        else 
            let el, rest = get_random_element l length in
            aux rest (length - 1) (el :: acc)
    in 
    aux l (List.length l) []
;;
        

permutation ["a"; "b"; "c"; "d"; "e"; "f"];;
