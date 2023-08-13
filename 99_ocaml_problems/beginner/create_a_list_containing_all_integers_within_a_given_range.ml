let range f s = 
    let rec aux acc f s = 
        if f > s then acc
        else aux (f :: acc) (f + 1) s
    in
    List.rev (aux [] f s)
;;

let range a b =
    let rec aux a b =
      if a > b then [] 
      else a :: aux (a + 1) b
    in
    if a > b then List.rev (aux b a) 
    else aux a b;;

(* Tail recursive *)
let range a b =
    let rec aux acc high low =
      if high >= low then
        aux (high :: acc) (high - 1) low
      else acc
    in
      if a < b then aux [] b a else List.rev (aux [] a b);;

range 4 9;;

