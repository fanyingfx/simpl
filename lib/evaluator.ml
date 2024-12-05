open Ast

exception TypeError of string
exception RuntimeError of string

let type_error s = raise (TypeError s)
let runtime_error s = raise (RuntimeError s)
let unbound_var_err = "Unbound variable"
let bop_err = "Operator and operand type mismatch"
let if_guard_err = "guard of if must have type bool"
let annotation_error = "Let expression type mismatch"
let if_branch_err = ""

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
  | Let (y, t, e1, e2) ->
    let e1' = subst e1 v x in
    (* rebinding so stop substitute *)
    if x = y then Let (y, t, e1', e2) else Let (y, t, e1, subst e2 v x)
  | If (e1, e2, e3) -> If (subst e1 v x, subst e2 v x, subst e3 v x)
;;

let empty = []
let extend env x t = (x, t) :: env

let lookup env e =
  match List.assoc_opt e env with
  | Some t -> t
  | None -> type_error unbound_var_err
;;

let rec typeof env e =
  match e with
  | Bool _ -> TBool
  | Int _ -> TInt
  | Var x -> lookup env x
  | Binop (op, e1, e2) -> typeof_binop env op e1 e2
  | Let (x, t, e1, e2) -> typeof_let env x t e1 e2
  | If (e1, e2, e3) -> typeof_if env e1 e2 e3
(* | _ -> failwith "TODO" *)

and typeof_binop env bop e1 e2 =
  match bop, typeof env e1, typeof env e2 with
  | Add, TInt, TInt -> TInt
  | Mult, TInt, TInt -> TInt
  | Leq, TInt, TInt -> TBool
  | _ -> type_error bop_err

and typeof_let env x t e1 e2 =
  let t' = typeof env e1 in
  if t = t'
  then (
    let env' = extend env x t' in
    typeof env' e2)
  else type_error annotation_error

and typeof_if env e1 e2 e3 =
  let t1 = typeof env e1 in
  if t1 <> TBool
  then type_error if_guard_err
  else (
    let t2 = typeof env e2 in
    let t3 = typeof env e3 in
    if t2 = t3 then t2 else type_error if_branch_err)
;;

let typecheck e =
  ignore (typeof empty e);
  e
;;

let rec eval (e : expr) : expr =
  match e with
  | Int _ | Bool _ -> e
  | Var _ -> runtime_error unbound_var_err
  | Binop (bop, e1, e2) -> eval_bop bop e1 e2
  | Let (x, _, e1, e2) -> eval_let x e1 e2
  | If (e1, e2, e3) -> eval_if e1 e2 e3

and eval_let x e1 e2 =
  let v1 = eval e1 in
  let e2' = subst e2 v1 x in
  eval e2'

and eval_bop bop e1 e2 =
  match bop, eval e1, eval e2 with
  | Add, Int a, Int b -> Int (a + b)
  | Mult, Int a, Int b -> Int (a * b)
  | Leq, Int a, Int b -> Bool (a <= b)
  | _ -> runtime_error bop_err

and eval_if e1 e2 e3 =
  match eval e1 with
  | Bool true -> eval e2
  | Bool false -> eval e3
  | _ -> runtime_error if_guard_err
;;

let interp (s : string) : string = s |> parse |> typecheck |> eval |> string_of_val
let () = assert (interp "2+2" = "4")
