let split_char = [ ' '; ','; ';'; '.'; '!'; '?'; ':'; '-' ]
let print_list l = List.iter print_char l

let print_list_of_tuple l =
  List.iter
    (fun (a, b) ->
      print_list a;
      print_string " ";
      print_int b;
      print_newline ())
    l

let string_of_file fn =
  let file = open_in fn in
  let rec process_lines acc =
    try
      let line = String.lowercase_ascii (input_line file) in
      process_lines (acc ^ line ^ " ")
    with e ->
      close_in_noerr file;
      acc
  in
  process_lines ""

let rec list_char_of_string s =
  match s with
  | "" -> []
  | _ -> s.[0] :: list_char_of_string (String.sub s 1 (String.length s - 1))

let list_word_of_list_char lc =
  let rec aux lc word acc =
    match lc with
    | [] -> acc
    | h :: t ->
        if List.mem h split_char then
          aux t [] ((List.rev word, 1) :: ([ h ], 1) :: acc)
        else aux t (h :: word) acc
  in
  aux lc [] []

let reduce_by_key lw =
  let rec aux acc_w acc_l rest =
    match rest with
    | [] -> (acc_w, acc_l)
    | h :: tail ->
        if fst h = fst acc_w then aux (fst h, snd h + snd acc_w) acc_l tail
        else aux acc_w (h :: acc_l) tail
  in
  let rec aux2 acc lw =
    match lw with
    | [] -> acc
    | h :: t ->
        let w, l = aux h [] t in
        aux2 (w :: acc) l
  in
  aux2 [] lw

let () =
  let fn = "lorem_ipsum_small.txt" in
  let lc = list_char_of_string (string_of_file fn) in
  let lw = list_word_of_list_char lc in
  let rbk = reduce_by_key lw in
  print_list_of_tuple rbk
