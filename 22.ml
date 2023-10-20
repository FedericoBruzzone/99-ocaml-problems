(*
  22. Create a list containing all integers within a given range. (easy)

  If first argument is greater than second, produce a list in decreasing order.
*)

let range f t =
  let mn, mx, rev = (min f t, max f t, f < t) in
  let res = List.init (mx - mn + 1) (fun x -> x + mn) in
  match rev with false -> res |> List.rev | true -> res

let range f t =
  let mn, mx, rev = (min f t, max f t, f < t) in
  let rec aux op f n =
    match n with 0 -> [] | _ -> op f 1 :: aux op (op f 1) (n - 1)
  in
  match rev with
  | false -> aux ( - ) (mx + 1) (mx - mn + 1)
  | true -> aux ( + ) (mn - 1) (mx - mn + 1)
;;

range 4 9;;
range 9 4
