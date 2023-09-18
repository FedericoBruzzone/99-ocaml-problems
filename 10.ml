let encode l =
    let rec aux acc count = function
        | [] -> []
        | [ x ] -> (count + 1, x) :: acc
        | a :: (b :: _ as rest) ->
            if a = b then
                aux acc (count + 1) rest
            else
                aux ((count + 1, a) :: acc) 0 rest
    in
    List.rev (aux [] 0 l)
;;

let pack l =
    let rec aux curr acc = function
        | [] -> []
        | [ x ] -> (x :: curr) :: acc
        | a :: (b :: _ as rest) ->
            if a = b then
                aux (a :: curr) acc rest
            else
                aux [] ((a :: curr) :: acc) rest
    in
    List.rev (aux [] [] l);;

let encode l =
    List.map (fun x -> (List.length l, List.hd l)) (pack l)
;;

encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;

