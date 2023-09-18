type 'a rle =
    | One of 'a
    | Many of int * 'a
;;

let decode (l : 'a rle list) =
    let rec get_many n x =
        match n with
        | 0 -> []
        | _ -> x :: get_many (n - 1) x
    in
    let rec aux acc l =
        match l with
        | [] -> acc
        | One x :: rest -> aux (x :: acc) rest
        | Many (n, x) :: rest -> aux (get_many n x @ acc) rest
    in
    aux [] l |> List.rev
;;

decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;
