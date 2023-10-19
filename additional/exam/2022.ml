let continuous_regions l =
  let rec aux acc acc_l l =
    match l with
    | [] -> acc_l
    | h :: [] -> (acc + 1) :: acc_l
    | f :: s :: t ->
        if f = s then aux (acc + 1) acc_l (s :: t)
        else aux 0 ((acc + 1) :: acc_l) t
  in
  aux 0 [] l
;;

continuous_regions [ 1; 1; 1; 1; 2; 2; 3; 5; 5; 5; 7; 7; 9; 10; 10; 10; 10 ]
