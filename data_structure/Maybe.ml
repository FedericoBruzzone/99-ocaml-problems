open Monad

module Maybe : Monad = struct
  type 'a t = 'a option

  let return (x : 'a) : 'a t = Some x

  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    match m with None -> None | Some x -> f x

  let ( >>= ) = bind

  let match_ x =
    match x with None -> print_endline "None" | Some x -> print_endline "Some"

  let ( + ) (x : int t) (y : int t) : int t =
    x >>= fun x ->
    y >>= fun y -> return (Stdlib.( + ) x y)

  let test = return 1 + return 2
end

let () =
  let open Maybe in
  let ( + ) (x : int t) (y : int t) : int t =
    x >>= fun x ->
    y >>= fun y -> return (Stdlib.( + ) x y)
  in
  let x = return 1 in
  let y = return 2 in
  let z = return 3 in
  let res = x + y + z in
  let match_2 x =
    match x with
      None -> print_endline "None"
    | Some x -> print_endline "Some" in
  let () = match_ res in         (* It works *)
  let res_s = Some 1 + Some 2 in (* Not work *)
  let () = match_2 res in        (* Not work *)
  ()
