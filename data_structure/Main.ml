open Opt
open Maybe

let () =
  let open Maybe in
  let a = Maybe 10 in
  let res = Opt.get a in
  print_int res
