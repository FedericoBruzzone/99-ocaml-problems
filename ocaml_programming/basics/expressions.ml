5 / 2;;

5. /. 2.;;

2 * 5;;

2. *. 5.;;

5 - 2;;

5. -. 2.;;

5 + 2;;

5. +. 2.;;

5 mod 2;;

5. ** 2.;;

int_of_float 5.;;

"abc" ^ "def";;

string_of_int 5;;

int_of_string "5";;

String.make 1 'z';;

"abc".[0];;

assert (1 = 1);;

if 3 + 5 > 2 then "yay!" else "boo!";;

4 + (if 'a' = 'b' then 1 else 2);;

if 1 = 1 then if 2 = 2 then 3 else 4 else 5;;

let x = 42;;

let x = 42 in x + 1;;

let a = "big";;
let b = "red";;
let c = a ^ b;;

let a = "big" in
let b = "red" in
let c = a ^ b in
c;;

let x = 42 in
  (* y is not meaningful here *)
  x + (let y = "3110" in
         (* y is meaningful here *)
         int_of_string y);;


(* possibility 1 *)
let x = 5 in
  ((let x = 6 in 6) + 5);;

(* possibility 2 *)
let x = 5 in
  ((let x = 6 in 5) + 5);;

(* possibility 3 *)
let x = 5 in
  ((let x = 6 in 6) + 6);;

let f x = x * x;;
let f y = y * y;;

let x = 5 in (let x = 6 in x) + x;;
let x = 5 in (let y = 6 in y) + x;;
