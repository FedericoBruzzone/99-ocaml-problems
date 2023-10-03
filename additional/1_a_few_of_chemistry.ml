let print_list_of_tuples l =
  List.iter (fun (x, y) -> Printf.printf "%s : %d\n" x y) l

let alkaline_earth_metals =
  [
    ("beryllium", 4);
    ("magnesium", 12);
    ("calcium", 20);
    ("strontium", 38);
    ("barium", 56);
    ("radium", 88);
  ]

let noble_gases =
  [
    ("helium", 2);
    ("neon", 10);
    ("argon", 18);
    ("krypton", 36);
    ("xenon", 54);
    ("radon", 86);
  ]

let rec highest_atomic_number l =
  match l with
  | [] -> -1
  | [ x ] -> snd x
  | f :: s :: tl ->
      if snd f < snd s then highest_atomic_number (s :: tl)
      else highest_atomic_number (f :: tl)

let rec sort_atomic_number l =
  let inner l =
    match l with
    | f :: s :: tl ->
        if snd f < snd s then s :: sort_atomic_number (f :: tl)
        else f :: sort_atomic_number (s :: tl)
    | f -> f
  in
  if l = inner l then l else sort_atomic_number (inner l)

let rec sort_atomic_number_by_name l =
  let inner l =
    match l with
    | f :: s :: tl ->
        if fst f > fst s then s :: sort_atomic_number_by_name (f :: tl)
        else f :: sort_atomic_number_by_name (s :: tl)
    | f -> f
  in
  if l = inner l then l else sort_atomic_number_by_name (inner l)

let rec merge_lists list1 list2 =
  match (list1, list2) with
  | [], list2 -> list2
  | list1, [] -> list1
  | hd1 :: tl1, hd2 :: tl2 -> hd1 :: hd2 :: merge_lists tl1 tl2

(* Tail recursive *)
let merge_lists l1 l2 =
  let rec inner acc l1 l2 =
    match (l1, l2) with
    | [], [] -> acc
    | [], hd :: tl -> inner (hd :: acc) [] tl
    | hd :: tl, [] -> inner (hd :: acc) tl []
    | h1 :: t1, h2 :: t2 -> inner (h1 :: h2 :: acc) t1 t2
  in
  inner [] l1 l2

let () =
  Printf.printf "-------------------- 1 --------------------\n";

  print_list_of_tuples alkaline_earth_metals;
  Printf.printf "\n";

  Printf.printf "Max: %d\n" (highest_atomic_number alkaline_earth_metals);
  Printf.printf "\n";

  let sort_alkaline_earth_metals = sort_atomic_number alkaline_earth_metals in
  print_list_of_tuples sort_alkaline_earth_metals;
  Printf.printf "\n";

  let merge_list = merge_lists alkaline_earth_metals noble_gases in
  let sort_alkaline_earth_metals_by_name =
    sort_atomic_number_by_name merge_list
  in
  print_list_of_tuples sort_alkaline_earth_metals_by_name;
  Printf.printf "\n";
