(*
  46 & 47. Truth tables for logical expressions (2 variables). (medium)

  Define a function, table2 which returns the truth table of a given logical
  expression in two variables (specified as arguments). The return value must
  be a list of triples containing (value_of_a, value_of_b, value_of_expr).
*)

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr
(* (a ∨ b) ∧ (a ∧ b) -> And (Or (Var "a", Var "b"), And (Var "a", Var "b"))*)

let rec eval t1 t2 e =
  let real_value x = if x = fst t1 then snd t1 else snd t2 in
  match e with
  | Var x -> real_value x
  | Not ne -> not (eval t1 t2 ne)
  | And (er, el) -> eval t1 t2 er && eval t1 t2 el
  | Or (er, el) -> eval t1 t2 er || eval t1 t2 el

let product l1 l2 =
  match (l1, l2) with
  | [], _ -> []
  | _, [] -> []
  | _, _ -> List.flatten (List.map (fun x -> List.map (fun y -> (x, y)) l2) l1)

let table2 v1 v2 e =
  let tf = product [ true; false ] [ true; false ] in
  let v1v2 = product [ v1; v2 ] [ v1; v2 ] in
  let t1t2l = List.map2 (fun x y -> ((fst x, fst y), (snd x, snd y))) v1v2 tf in
  List.map
    (fun x ->
      let t1 = fst x in
      let t2 = snd x in
      (snd t1, snd t2, eval t1 t2 e))
    t1t2l
;;

table2 "a" "b" (And (Var "a", Or (Var "a", Var "b")))
