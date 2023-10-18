(*
Beyond the well-known Celsius and Fahrenheit, there are other six temperature scales: Kelvin, Rankine, Delisle, Newton, Réaumur, and Rømer (Look at:

http://en.wikipedia.org/wiki/Comparison_of_temperature_scales

to read about them).

    1. Write a function that given a pure number returns a conversion table for it among any of the 8 scales.
    2. Write a function that given a temperature in a specified scale returns a list of all the corresponding temperatures in the other scales, note that the scale must be specified.

Hint. Define a proper datatype for the temperature.
*)

type scale =
  | Celsius
  | Fahrenheit
  | Kelvin
  | Rankine
  | Delisle
  | Newton
  | Reaumur
  | Romer

type temperature = { scl : scale; v : float }

let from_celsius_to_any t = function
  | Celsius -> t
  | Fahrenheit -> { scl = Fahrenheit; v = (t.v *. 9. /. 5.) +. 32. }
  | Kelvin -> { scl = Kelvin; v = t.v +. 273.15 }
  | Rankine -> { scl = Rankine; v = (t.v *. 9. /. 5.) +. 491.67 }
  | Delisle -> { scl = Delisle; v = (100. -. t.v) *. 3. /. 2. }
  | Newton -> { scl = Newton; v = t.v *. 33. /. 100. }
  | Reaumur -> { scl = Reaumur; v = t.v *. 4. /. 5. }
  | Romer -> { scl = Romer; v = (t.v *. 21. /. 40.) +. 7.5 }

let from_any_to_celsius t =
  match t.scl with
  | Celsius -> t
  | Fahrenheit -> { scl = Celsius; v = (t.v -. 32.) *. 5. /. 9. }
  | Kelvin -> { scl = Celsius; v = t.v -. 273.15 }
  | Rankine -> { scl = Celsius; v = (t.v -. 491.67) *. 5. /. 9. }
  | Delisle -> { scl = Celsius; v = 100. -. (t.v *. 2. /. 3.) }
  | Newton -> { scl = Celsius; v = t.v *. 100. /. 33. }
  | Reaumur -> { scl = Celsius; v = t.v *. 5. /. 4. }
  | Romer -> { scl = Celsius; v = (t.v -. 7.5) *. 40. /. 21. }

let string_of_scale = function
  | Celsius -> "Cel"
  | Fahrenheit -> "Fah"
  | Kelvin -> "Kel"
  | Rankine -> "Ran"
  | Delisle -> "Del"
  | Newton -> "New"
  | Reaumur -> "Rea"
  | Romer -> "Rom"

let pad s n =
  let len = String.length s in
  if len >= n then s
  else
    let pad = String.make (n - len) ' ' in
    pad ^ s

let make_row t =
  let scales =
    [ Celsius; Fahrenheit; Kelvin; Rankine; Delisle; Newton; Reaumur; Romer ]
  in
  List.map (fun s -> from_celsius_to_any (from_any_to_celsius t) s) scales

let pure_to_table n =
  let scales =
    [ Celsius; Fahrenheit; Kelvin; Rankine; Delisle; Newton; Reaumur; Romer ]
  in
  let rec aux scales =
    match scales with
    | [] -> []
    | hd :: tl ->
        let t = { scl = hd; v = n } in
        let row = make_row t in
        row :: aux tl
  in
  aux scales

let print_table tab =
  List.iter
    (fun s ->
      Printf.printf "%s" (pad (Printf.sprintf "%s" (string_of_scale s)) 10))
    [ Celsius; Fahrenheit; Kelvin; Rankine; Delisle; Newton; Reaumur; Romer ];
  Printf.printf "\n";
  List.iter
    (fun row ->
      List.iter
        (fun el -> Printf.printf "%s" (pad (Printf.sprintf "%.2f" el.v) 10))
        row;
      Printf.printf "\n")
    tab

let () =
  let table = pure_to_table 42.0 in
  print_table table;
  Printf.printf "\n"
