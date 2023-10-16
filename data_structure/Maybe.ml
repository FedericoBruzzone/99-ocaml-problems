module Maybe = struct
  type 'a t = Nothing | Maybe of 'a

  let return (a : 'a) : 'a t = Maybe a

  let bind (m : 'a t) (f : 'a -> 'b t) : 'b t =
    match m with Nothing -> Nothing | Maybe a -> f a
end
