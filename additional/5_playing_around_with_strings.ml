(*
Define the following functions/operators on strings:

    1. is_palindrome: string → bool that checks if the string is palindrome, a string is palindrome when the represented sentence can be read the same way in either directions in spite of spaces, punctual and letter cases, e.g., detartrated, "Do geese see God?", "Rise to vote, sir.", ...
    2. operator (-): string → string → string that subtracts the letters in a string from the letters in another string, e.g., "Walter Cazzola"-"abcwxyz" will give "Wlter Col" note that the operator - is case sensitive
    3. anagram : string → string list → boolean that given a dictionary of strings, checks if the input string is an anagram of one or more of the strings in the dictionary
*)

let rec string_reverse s =
  if String.length s = 0 then ""
  else
    string_reverse (String.sub s 1 (String.length s - 1))
    ^ String.make 1 (String.get s 0)

let filter s =
  let s = String.map (fun c -> if c = ' ' then '_' else c) s in
  let s = String.map (fun c -> if c = ',' then '_' else c) s in
  let s = String.map (fun c -> if c = '?' then '_' else c) s in
  let s = String.map (fun c -> if c = '.' then '_' else c) s in
  let s = String.map (fun c -> if c = '!' then '_' else c) s in
  let s = String.map (fun c -> if c = '\'' then '_' else c) s in
  let s = String.map (fun c -> if c = '-' then '_' else c) s in
  let s = String.map (fun c -> if c = '_' then '_' else c) s in
  s

let is_palindrome s =
  let x = String.lowercase_ascii s |> filter in
  x = string_reverse x

let remove_char s x =
  let rec aux s acc =
    if String.length s = 0 then acc
    else if String.get s 0 = x then
      aux (String.sub s 1 (String.length s - 1)) acc
    else
      aux
        (String.sub s 1 (String.length s - 1))
        (String.make 1 (String.get s 0) ^ acc)
  in
  aux s ""

let ( - ) s rem =
  let rec aux s rem =
    if String.length rem = 0 then s
    else
      aux
        (remove_char s (String.get rem 0))
        (String.sub rem 1 (String.length rem - 1))
  in
  aux s rem |> string_reverse

let string_filter f s =
  let rec aux f s acc =
    if String.length s = 0 then acc
    else if f (String.get s 0) then
      aux f
        (String.sub s 1 (String.length s - 1))
        (String.make 1 (String.get s 0) ^ acc)
    else aux f (String.sub s 1 (String.length s - 1)) acc
  in
  aux f s ""

(* let anagram s l = *)

let () =
  print_string "is_palindrome \"Do geese see God?\" = ";
  print_endline (string_of_bool (is_palindrome "Do geese see God?"));
  print_newline ();
  print_string "is_palindrome \"Do you see God?\" = ";
  print_endline (string_of_bool (is_palindrome "Do you see God?"));
  print_newline ();
  print_string ("Federico Bruzzone" - "ricoBruzz");
  print_newline ();
  print_string (string_filter (fun c -> c = 'a') "abracadabra")
