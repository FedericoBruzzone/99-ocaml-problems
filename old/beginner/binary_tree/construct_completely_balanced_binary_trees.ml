type 'a binary_tree =
      | Empty
      | Node of 'a * 'a binary_tree * 'a binary_tree;;

let cbal_tree n =
    let rec aux n =
        if n = 0 then 
            [Empty]
        else if n mod 2 = 1 then
            let t = aux (n / 2) in
            List.flatten (List.map (fun l -> List.map (fun r -> Node ('x', l, r)) t) t)
        else
            let t1 = aux (n / 2 - 1) in
            let t2 = aux (n / 2) in
            List.flatten (List.map (fun l -> List.map (fun r -> Node ('x', l, r)) t2) t1) @
            List.flatten (List.map (fun l -> List.map (fun r -> Node ('x', l, r)) t1) t2)
    in aux n;;

let example_tree =
      Node ('a', Node ('b', Node ('d', Empty, Empty), 
                            Node ('e', Empty, Empty)),
                 Node ('c', Empty, 
                            Node ('f', Node ('g', Empty, Empty), 
                                       Empty)));;
