let rec at k = function
    | [] -> None
    | h :: t -> if k = 0 then Some h else at (k - 1) t
;;

(* List.nth ["a"; "b"; "c"; "d"; "e"] 2;; *)
(* List.nth ["a"] 2;; *)
at 2 ["a"; "b"; "c"; "d"; "e"];;
at 2 ["a"];;
