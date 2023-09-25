let lotto_select n m =
    let generate_random m = Random.int m in
    let rec aux count acc = 
        if count = 0 then acc
        else aux (count - 1) (generate_random m :: acc) 
    in 
    aux n []
;;



lotto_select 6 49;;
