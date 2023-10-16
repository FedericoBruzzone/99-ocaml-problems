open IMonad

module MakeMonad (M : IMonad) = struct
  type 'a t = 'a M.t

  let return = M.return
  let bind = M.bind
  let ( >>= ) = M.bind
end

