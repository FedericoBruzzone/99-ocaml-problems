(*
  24. Lotto: Draw N different random numbers from the set 1..M. (easy)

  The selected numbers shall be returned in a list.
*)

let range f t =
  let mn, mx, rev = (min f t, max f t, f < t) in
  let res = List.init (mx - mn + 1) (fun x -> x + mn) in
  match rev with false -> res |> List.rev | true -> res

let rand_select list n =
  let rec extract acc n = function
    | [] -> raise Not_found
    | h :: t -> if n = 0 then (h, acc @ t) else extract (h :: acc) (n - 1) t
  in
  let extract_rand list len = extract [] (Random.int len) list in
  let rec aux n acc list len =
    if n = 0 then acc
    else
      let picked, rest = extract_rand list len in
      aux (n - 1) (picked :: acc) rest (len - 1)
  in
  let len = List.length list in
  aux (min n len) [] list len

let lotto_select n m = rand_select (range 1 m) n;;

lotto_select 6 49
