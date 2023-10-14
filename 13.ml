(*
  13. Run-length encoding of a list (direct solution). (medium)

  Implement the so-called run-length encoding data compression method directly.
  I.e. don't explicitly create the sublists containing the duplicates, as in
  problem "Pack consecutive duplicates of list elements into sublists", but
  only count them. As in problem "Modified run-length encoding", simplify the
  result list by replacing the singleton lists (1 X) by X.
*)

type 'a rle = One of 'a | Many of int * 'a

let encode list =
  let rle count n = if count = 0 then One n else Many (count + 1, n) in
  let rec aux count acc = function
    | [] -> []
    | [ n ] -> rle count n :: acc
    | f :: (s :: _ as t) ->
        if f = s then aux (count + 1) acc t else aux 0 (rle count f :: acc) t
  in
  aux 0 [] list |> List.rev
;;

encode [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
