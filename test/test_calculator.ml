
(* test.ml *)

open Alcotest
open Calculator.Evaluator
let myexn s =  fun ()-> ignore (interp s)
(* let check_fail err_msg expr_str = 
  check_raises "should faill" (Failure err_msg) (myexn expr_str) *)
let check_type_err  err_msg expr_str = 
  check_raises "should faill" (TypeError err_msg) (myexn expr_str)
let test_addition () =
  check string "add 2 + 2" "4" (interp "2+2")
let test_mul_then_add () =
  check string "mul then add" "14" (interp "2*2+10")
let test_add_then_mul () =
  check string "add then mul" "22" (interp "2+2*10")
let test_let () = 
  check string "let" "22" (interp "let x:int = 22 in x")
let test_lets () = 
  check string "lets" "22" (interp "let x:int =0 in let x:int = 22 in x")

let test_if1 () =
  check string "if1" "22" (interp "if true then 22 else 0")

let test_true () =
  check string "true" "true" (interp "true")

let test_leq () =
  check string "leq" "true" (interp "1<=1")

let test_if2 () =
  check string "if2" "22" (interp "if 1+2 <= 3+4 then 22 else 0")

let test_if3 () =
  check string "if3" "22" (interp "if 1+2 <= 3*4 then let x : int= 22 in x else 0")

let test_letif () =
  check string "letif" "22" (interp "let x:bool= 1+2 <= 3*4 in if x then 22 else 0")

let test_ty_plus () =
   check_type_err bop_err "1+true"
let test_ty_mult () =
  check_type_err bop_err  "1 * false"

let test_ty_leq () =
  check_type_err bop_err "true <= 1"

let test_if_guard () =
  check_type_err if_guard_err "if 1 then 2 else 3"

(* let test_if_branch () =
  check string "if branch" if_branch_err (interp "if true then 2 else false") *)

let test_unbound () =
  check_type_err unbound_var_err "x"
let () =
  let open Alcotest in
  run "Test Suite" [
    "Arithmetic", [
      test_case "Addition" `Quick test_addition;
      test_case "MulThenAdd" `Quick test_mul_then_add;
      test_case "AddThenMul" `Quick test_add_then_mul;
      test_case "Let"   `Quick test_let;
      test_case "Lets"   `Quick test_lets;
      test_case "If1" `Quick test_if1;
      test_case "True" `Quick test_true;
      test_case "Leq" `Quick test_leq;
      test_case "If2" `Quick test_if2;
      test_case "If3" `Quick test_if3;
      test_case "LetIf" `Quick test_letif;
      test_case "TyPlus" `Quick test_ty_plus;
      test_case "TyMult" `Quick test_ty_mult;
      test_case "TyLeq" `Quick test_ty_leq;
      test_case "IfGuard" `Quick test_if_guard;
      (* test_case "IfBranch" `Quick test_if_branch; *)
      test_case "Unbound" `Quick test_unbound;
    ]
  ]
