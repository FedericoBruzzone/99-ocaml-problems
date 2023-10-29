open MakeMonad

module MList = struct
  type 'a t = 'a list

  let return (x : 'a) : 'a t = [ x ]
  let bind (xs : 'a t) (f : 'a -> 'b t) : 'b t =
    let rec aux (xs : 'a t) (acc : 'b t) : 'b t =
      match xs with
      | [] -> acc
      | x :: xs -> aux xs (acc @ (f x))
    in
    aux xs []
end

module MonadicList = struct
  include MakeMonad(MList)
end

let () =
  let open MonadicList in
  let l1 : int MonadicList.t = [ 5; 2; 3; 4; 1 ] in
  let last_element = l1 >>= (fun x -> (x, 1) :: []) in
  ()




