(*
  28. Sorting a list of lists according to length of sublists. (medium)

  1. We suppose that a list contains elements that are lists themselves.
     The objective is to sort the elements of this list according to their
     length. E.g. short lists first, longer lists later, or vice versa.

  2. Again, we suppose that a list contains elements that are lists themselves.
     But this time the objective is to sort the elements of this list according
     to their length frequency; i.e., in the default, where sorting is done
     ascendingly, lists with rare lengths are placed first, others with a more
     frequent length come later.
*)

let quick_sort op l =
  let rec quick_sort l =
    match l with
    | [] -> []
    | h :: t ->
        let sx, dx = List.partition (fun x -> op x h) t in
        quick_sort sx @ [ h ] @ quick_sort dx
  in
  quick_sort l

let run_length_encoding l =
  let rec aux count acc = function
    | [] -> []
    | [ x ] -> (x, count + 1) :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (count + 1) acc t else aux 0 ((a, count + 1) :: acc) t
  in
  aux 0 [] l

let length_sort l = quick_sort (fun x y -> List.length x < List.length y) l

let frequency_sort l =
  let lengths = List.map List.length l in
  let freq = run_length_encoding (quick_sort (fun x y -> x < y) lengths) in
  let by_freq =
    List.map (fun list -> (List.assoc (List.length list) freq, list)) l
  in
  let sorted = quick_sort (fun x y -> fst x < fst y) by_freq in
  List.map snd sorted
;;

length_sort
  [
    [ "a"; "b"; "c" ];
    [ "d"; "e" ];
    [ "f"; "g"; "h" ];
    [ "d"; "e" ];
    [ "i"; "j"; "k"; "l" ];
    [ "m"; "n" ];
    [ "o" ];
  ]
;;

frequency_sort
  [
    [ "a"; "b"; "c" ];
    [ "d"; "e" ];
    [ "f"; "g"; "h" ];
    [ "d"; "e" ];
    [ "i"; "j"; "k"; "l" ];
    [ "m"; "n" ];
    [ "o" ];
  ]
