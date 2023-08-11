let encode l =
    let rec aux acc count = function
        | [] -> []
        | [ x ] -> (count + 1, x) :: acc
        | h1 :: (h2 :: _ as t) -> if h1 = h2 then aux acc (count + 1) t
                                  else aux ((count + 1, h1) :: acc) 0 t 
    in
    List.rev (aux [] 0 l)
;;

let pack l =
    let rec aux current acc = function
        | [] -> [] 
        | [ x ] -> (x :: current) :: acc
        | a :: (b :: _ as t) -> if a = b then aux (a :: current) acc t
                                else aux [] ((a :: current) :: acc) t  
    in
    List.rev (aux [] [] l);;

let encode l = 
    List.map (fun x -> (List.length l, List.hd l)) (pack l)
;;

encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;

