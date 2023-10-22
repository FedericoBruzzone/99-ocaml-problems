(*
  27. Group the elements of a set into disjoint subsets. (medium)

  1. In how many ways can a group of 9 people work in 3 disjoint subgroups of
     2, 3 and 4 persons? Write a function that generates all the possibilities
     and returns them in a list.

  2. Generalize the above function in a way that we can specify a list of group
     sizes and the function will return a list of groups.
*)

let group list sizes =
  let initial = List.map (fun size -> (size, [])) sizes in
  let prepend p list =
    let emit l acc = l :: acc in
    let rec aux emit acc = function
      | [] -> emit [] acc
      | ((n, l) as h) :: t ->
          let acc = if n > 0 then emit ((n - 1, p :: l) :: t) acc else acc in
          aux (fun l acc -> emit (h :: l) acc) acc t
    in
    aux emit [] list
  in
  let rec aux = function
    | [] -> [ initial ]
    | h :: t -> List.concat (List.map (prepend h) (aux t))
  in
  let all = aux list in
  let complete = List.filter (List.for_all (fun (x, _) -> x = 0)) all in
  List.map (List.map snd) complete
;;

group [ "a"; "b"; "c"; "d" ] [ 2; 1 ]
