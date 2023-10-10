open Str

type wf = {
  w : string;
  f : int
}

let split_line line =
  let lower_line = String.lowercase_ascii line in
  let words = Str.split (Str.regexp "[ ,.;:!?\t\n]+") lower_line in
  List.filter (fun w -> w <> "") words

let count_wf fn =
  let file = open_in fn in
  let rec process_lines wf_list =
    try
      let line = input_line file in
      let words = split_line line in
      List.iter (fun x -> print_string x) words
    with e ->
      close_in_noerr file;
      raise e
  in
  process_lines []



let () =
  let fn = "lorem_ipsum.txt" in
  count_wf fn

