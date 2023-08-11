let split l n = 
    let rec aux acc rest count = match count = n with
        | true -> acc, rest
        | false -> match rest with
            | [] -> acc, rest
            | h :: t -> aux (h :: acc) t (count + 1)
    in 
    (* List.rev (fst (aux [] l 0)), snd (aux [] l 0) *)
    let left, right = aux [] l 0 in
    List.rev left, right
;;

let split l n =
    let rec aux i acc = function
      | [] -> List.rev acc, []
      | h :: t as l -> if i = 0 then List.rev acc, l
                       else aux (i - 1) (h :: acc) t 
    in
    aux n [] l;;

split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
split ["a"; "b"; "c"; "d"] 5;;
