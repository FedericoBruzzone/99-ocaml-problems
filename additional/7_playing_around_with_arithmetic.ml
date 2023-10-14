open MyStack

module PolishNotation (Stack : IStack) = struct
  type num = int
  type op = Plus | Minus | Times | Divide | Pow
  type expr = Num of num | Unary of op * expr | Binary of expr * op * expr

  let is_sign x =
    match x with "+" | "-" | "*" | "/" | "**" -> true | _ -> false

  let op_of_string s =
    match s with
    | "+" -> Plus
    | "-" -> Minus
    | "*" -> Times
    | "/" -> Divide
    | "**" -> Pow
    | _ -> failwith "Invalid operator"

  let string_of_op op =
    match op with
    | Plus -> "+"
    | Minus -> "-"
    | Times -> "*"
    | Divide -> "/"
    | Pow -> "**"

  let print_expr (e : expr) : unit =
    let rec aux (e : expr) : unit =
      match e with
      | Num n -> print_int n
      | Unary (op, e) ->
          print_string "(";
          print_string (string_of_op op);
          aux e;
          print_string ")"
      | Binary (e1, op, e2) ->
          print_string "(";
          aux e1;
          print_string (string_of_op op);
          aux e2;
          print_string ")"
    in
    aux e;
    print_newline ()

  let expr_of_string (s : string) : expr =
    let s_list = String.split_on_char ' ' s in
    let rec aux (s_list : string list) (stack : expr Stack.t) : expr =
      match s_list with
      | [] -> fst (Stack.pop stack)
      | h :: t ->
          if is_sign h then
            let op = op_of_string h in
            let e1, stack' = Stack.pop stack in
            let e2, stack'' = Stack.pop stack' in
            let e = Binary (e2, op, e1) in
            aux t (Stack.push e stack'')
          else
            let e = Num (int_of_string h) in
            aux t (Stack.push e stack)
    in
    aux s_list (Stack.create () : expr Stack.t)

  let eval (e : expr) : int =
    let mlop_of_op op n1 n2 : int =
      match op with
      | Plus -> n1 + n2
      | Minus -> n1 - n2
      | Times -> n1 * n2
      | Divide -> n1 / n2
      | Pow -> int_of_float ((float_of_int n1) ** (float_of_int n2))
    in
    let rec aux = function
      Num n -> n
    | Unary (op, n) -> mlop_of_op op (aux n) 0
    | Binary (n1, op, n2) -> mlop_of_op op (aux n1) (aux n2)
    in
    aux e
end

module PolishNotationList = PolishNotation (StackList)
module PolishNotationArray = PolishNotation (StackArray)

let () =
  PolishNotationList.expr_of_string "1 2 + 3 *" |> PolishNotationList.print_expr;
  PolishNotationArray.expr_of_string "1 2 + 3 *" |> PolishNotationArray.print_expr;
  PolishNotationList.expr_of_string "1 2 + 3 *" |> PolishNotationList.eval |> print_int |> print_newline;
  PolishNotationArray.expr_of_string "1 2 + 3 *" |> PolishNotationArray.eval |> print_int |> print_newline;
  PolishNotationList.expr_of_string "1 2 + 3 * 4 **" |> PolishNotationList.print_expr;
  PolishNotationArray.expr_of_string "1 2 + 3 * 4 **" |> PolishNotationArray.print_expr;
  PolishNotationList.expr_of_string "1 2 + 3 * 4 **" |> PolishNotationList.eval |> print_int |> print_newline;
  PolishNotationArray.expr_of_string "1 2 + 3 * 4 **" |> PolishNotationArray.eval |> print_int |> print_newline;
