type 'a rle =
  | One of 'a
  | Many of int * 'a
;;

let encode l =
    let get_rle el count =
        match count with
        | 1 -> One el
        | _ -> Many (count, el)
    in
    let rec aux acc count = function
        | [] -> []
        | [ x ] -> (get_rle x (count + 1)) :: acc
        | f :: (s :: _ as t) ->
            match f = s with
            | true -> aux acc (count + 1) t
            | false -> aux ((get_rle f (count + 1)) :: acc) 0 t
    in
    List.rev (aux [] 0 l)
;;

encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;

