(* Typing_tests.ml *)

open FuncScript;;
open Test_utils;;


let new_std_type_context = fun () ->
  context_to_type_context (new_std_context ());;

let syntax_typing_test = generic_test (fun () -> reset_unclear_ids (); type_check_syntax (new_std_type_context()));;

let type_noop = syntax_typing_test
  "type_noop"
  (parse_syntax "")
  None_t;;

let type_expr_literal_int = syntax_typing_test
  "type_expr_literal_int"
  (parse_syntax "0")
  Int_t;;

let type_identity_function = syntax_typing_test
  "type_identity_function"
  (parse_syntax "(x -> x)")
  (Function_t([Unclear_t 0], Unclear_t 0));;

let () = set_typing_debug true;
  run_tests_and_display "TYPING" [
    (* type_noop; *)
    (* type_expr_literal_int; *)
    type_identity_function;
  ]