(* open Calculator.Interpreter  *)
(* open Calculator.Ast
open Calculator

let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast
;;

let string_of_val (e : expr) : string =
  match e with
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Binop _ | Let _ | Var _ | If _ -> failwith "precondition violated"
;;

let is_value : expr -> bool = function
  | Int _ | Bool _ -> true
  | Binop _ | Let _ | Var _ | If _ -> false
;;

let rec subst e v x =
  match e with
  | Var y -> if x = y then v else e
  | Int _ -> e
  | Bool _ -> e
  | Binop (bop, e1, e2) -> Binop (bop, subst e1 v x, subst e2 v x)
  | Let (y, e1, e2) ->
    let e1' = subst e1 v x in
    (* rebinding so stop substitute *)
    if x = y then Let (y, e1', e2) else Let (y, e1, subst e2 v x)
  | If (e1, e2, e3) -> If (subst e1 v x, subst e2 v x, subst e3 v x)
;;

let unbound_var_err = "Unbound variable"
let bop_err = "Operator and operand type mismatch"
let if_guard_err = "guard of if must have type bool"
let if_branch_err = ""

let rec step : expr -> expr = function
  | Var _ -> failwith unbound_var_err
  | Int _ | Bool _ -> failwith "Does not step"
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 -> step_binop bop e1 e2
  | Binop (bop, e1, e2) when is_value e1 -> Binop (bop, e1, step e2)
  | Binop (bop, e1, e2) -> Binop (bop, step e1, e2)
  | Let (x, e1, e2) when is_value e1 -> subst e2 e1 x
  | Let (x, e1, e2) -> Let (x, step e1, e2)
  | If (e1, e2, e3) when is_value e1 -> step_if e1 e2 e3
  | If (e1, e2, e3) -> If (step e1, e2, e3)

and step_if v1 e2 e3 =
  match v1 with
  | Bool true -> e2
  | Bool false -> e3
  | Int _ -> failwith if_guard_err
  | _ -> failwith "precondition violation"

and step_binop bop e1 e2 =
  match bop, e1, e2 with
  | Add, Int a, Int b -> Int (a + b)
  | Add, _, _ -> failwith bop_err
  | Mult, Int a, Int b -> Int (a * b)
  | Mult, _, _ -> failwith bop_err
  | Leq, Int a, Int b -> Bool (a <= b)
  | Leq, _, _ -> failwith bop_err
;;

let rec eval (e : expr) : expr = if is_value e then e else e |> step |> eval
let interp (s : string) : string = s |> parse |> eval |> string_of_val *) 
