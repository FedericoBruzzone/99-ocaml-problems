(*
type wf = {
  w : string;
  f : int
}

let count_wf fn =
  let file = open_in fn in
  let rec process_lines wf_list =
    try
      let line = input_line file in
      (* TODO *)
    with e ->
      close_in_noerr file;
      raise e
  in
  process_lines []



let () =
  let fn = "lorem_ipsum.txt" in
  count_wf fn
*)
