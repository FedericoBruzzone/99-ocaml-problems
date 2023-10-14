open Monad

module Maybe : Monad = struct
  type 'a t = 'a option

  let return (x : 'a) : 'a option = Some x

  let bind (m : 'a option) (f : 'a -> 'b option) : 'b option =
    match m with None -> None | Some x -> f x

  let ( >>= ) = bind

  let ( + ) x y =
    x >>= fun x ->
    y >>= fun y -> return (Stdlib.( + ) x y)

end

