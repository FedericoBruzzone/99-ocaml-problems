open Opt

let main =
  let el : 'a Opt.t = Maybe 0 in
  let get_el : 'a = Opt.get el in
  ()

let main2 =
  let open Opt in
  let g x = x >>= (fun x -> Printf.printf "%d\n" x; Maybe x) in
  let h g_value y = g_value;  y >>= (fun y -> Maybe y) in
  let f x = h (g x) in
  let f' = f (Maybe 0) in
  let v1 = get (f' (Maybe "abc")) in
  let _ = Printf.printf "%s\n" v1 in
  let v2 = get (f' (Maybe 1)) in (* does not work *)
  ()

let () = main
