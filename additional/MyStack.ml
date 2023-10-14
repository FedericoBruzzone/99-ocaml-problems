module type IStack = sig
  type 'a t

  val create : unit -> 'a t
  val is_empty : 'a t -> bool
  val push : 'a -> 'a t -> 'a t
  val pop : 'a t -> 'a * 'a t
end

module StackList : IStack = struct
  type 'a t = 'a list

  let create () = []
  let is_empty s = s = []
  let push x s = x :: s
  let pop s = match s with [] -> failwith "Empty stack" | x :: xs -> (x, xs)
end

module StackArray : IStack = struct
  type 'a t = { mutable arr : 'a array; mutable size : int }

  let create () = { arr = Array.make 10 (Obj.magic 0); size = 0 }
  let is_empty s = s.size = 0

  let push x s =
    s.arr.(s.size) <- x;
    s.size <- s.size + 1;
    s

  let pop s =
    s.size <- s.size - 1;
    (s.arr.(s.size), s)
end
