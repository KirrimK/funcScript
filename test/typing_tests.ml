(* Typing_tests.ml *)

open FuncScript;;
open Test_utils;;


let new_std_type_context = fun () ->
  context_to_type_context (new_std_context ());;

let syntax_typing_test = generic_test (fun () -> type_check_syntax (new_std_type_context()));;

let type_noop = syntax_typing_test
  "type_noop"
  FuncScript__.FSsyntax.STAT_NOOP
  None_t;;

let type_expr_literal_int = syntax_typing_test
  "type_expr_literal_int"
  (FuncScript__.FSsyntax.STAT_EXPR(FuncScript__.FSsyntax.EXPR_LITERAL(FuncScript__.FSsyntax.LITERAL_INT(1))))
  Int_t;;

let () = run_tests_and_display "TYPING" [
  type_noop;
  type_expr_literal_int;
]