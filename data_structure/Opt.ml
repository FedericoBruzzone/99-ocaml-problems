open MakeMonad
open Maybe

module Opt = struct
  include MakeMonad (Maybe)

  let get (m : 'a t) : 'a =
    match m with Nothing -> failwith "Nothing" | Maybe a -> a
end
