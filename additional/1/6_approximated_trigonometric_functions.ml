(*

sin(x) can be approximate by the Taylor's series: sinx= x - x^3/3! + x^5/5! ...

Similarly you can approximate all the trigonometric and transcendent functions (look at:

http://en.wikipedia.org/wiki/Taylor_series).

Let's write a module to implement sin x n by using the Taylor's series (where n is the level of approximation, i.e., 1 only one item, 2 two items, 3 three items and so on). Do the same with cosine, tangent, logarithm and so on.

Let's compare your functions with those implemented in the pervasive module at the growing of the approximation level.
*)

(* Tail recursive *)
let fact n =
  let rec fact' n acc = if n <= 1 then acc else fact' (n - 1) (acc * n) in
  fact' n 1

(* Non tail recursive, but it is very easy to implement it tail recursive *)
let rec sin x n =
  match n with
  | 0 -> x
  | n ->
      let sign = match n mod 2 with 0 -> 1. | _ -> -1. in
      sign
      *. ((x ** float_of_int ((n * 2) + 1)) /. float_of_int (fact ((n * 2) + 1)))
      +. sin x (n - 1)

let () =
  (* Pervasives is deprecated *)
  print_int (fact 10);
  print_newline ();
  Printf.printf "Stdlib: %f\n" (Stdlib.sin 2.);
  Printf.printf "My sin: %f\n" (sin 2. 10)
