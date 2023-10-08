(*
Write the matrix datatype with the following operations:

    1. A function zeroes to construct a matrix of size n×m filled with zeros.
    2. A function identity to construct the identity matrix (the one with all 0s but the 1s on the diagonal) of given size.
    3. A function init to construct a square matrix of a given size n filled with the first n×n integers.
    4. A function transpose that transposes a generic matrix independently of its size and content.
    5. The basics operators + and * that adds and multiplies two matrices non necessarily squared.
*)

type 'a matrix = 'a list list

let rec list_of_zeroes = function 0 -> [] | n -> 0 :: list_of_zeroes (n - 1)

let rec make_zeroes n m =
  match n with 0 -> [] | n -> list_of_zeroes m :: make_zeroes (n - 1) m

let rec make_identity n m =
  match n with
  | 0 -> []
  | n ->
      let rec make_row m =
        match m with
        | 0 -> []
        | m -> if m = n then 1 :: make_row (m - 1) else 0 :: make_row (m - 1)
      in
      make_row m :: make_identity (n - 1) m

let make_init n =
  let rec make_init' n m acc =
    match n with
    | 0 -> []
    | n ->
        let rec make_row m acc2 =
          match m with
          | 0 -> []
          | m -> (acc2 + acc) :: make_row (m - 1) (acc2 + 1)
        in
        make_row m 0 :: make_init' (n - 1) m (acc + m)
  in
  make_init' n n 1

let transpose m =
  let rec transpose' m acc =
    match m with
    | [] -> acc (* empty matrix *)
    | [] :: _ -> acc (* empty row *)
    | m ->
        let column = List.map List.hd m in
        let rest = List.map List.tl m in
        transpose' rest (column :: acc)
  in
  transpose' m [] |> List.rev

let ( + ) m1 m2 =
  let rec add m1 m2 =
    match (m1, m2) with
    | [], [] -> []
    | m1, [] -> m1
    | [], m2 -> m2
    | h1 :: t1, h2 :: t2 -> List.map2 (fun x y -> x + y) h1 h2 :: add t1 t2
    (* | h1 :: t1, h2 :: t2 ->
       (List.fold_left2
         (fun acc x y -> x + y :: acc)
         [] h1 h2 |> List.rev) :: add t1 t2 *)
    (* tail recursive *)
  in
  add m1 m2

let ( * ) m1 m2 =
  let rec mul m1 m2 =
    match (m1, m2) with
    | [], [] -> []
    | m1, [] -> m1
    | [], m2 -> m2
    | h1 :: t1, h2 :: t2 -> List.map2 (fun x y -> x * y) h1 h2 :: mul t1 t2
    (* | h1 :: t1, h2 :: t2 ->
       (List.fold_left2
         (fun acc x y -> x * y :: acc)
         [] h1 h2 |> List.rev) :: mul t1 t2 *)
    (* tail recursive *)
  in
  mul m1 m2

let print_matrix m =
  List.iter
    (fun row ->
      List.iter
        (fun x ->
          print_int x;
          print_string " ")
        row;
      print_newline ())
    m

let () =
  print_matrix (make_zeroes 3 3);
  print_newline ();
  print_matrix (make_identity 3 3);
  print_newline ();
  print_matrix (make_init 3);
  print_newline ();
  print_matrix (transpose (make_init 3));
  print_newline ();
  print_matrix (make_init 3 + make_init 3);
  print_newline ();
  let m1 = [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ] ] in
  let m2 = [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ] in
  print_matrix (m1 + m2);
  print_newline ();
  print_matrix (make_init 3 * make_init 3);
  print_newline ();
  let m1 = [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ] ] in
  let m2 = [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ] in
  print_matrix (m1 * m2);
