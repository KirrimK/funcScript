(* Parser_tests.ml*)

open Test_utils;;
open FuncScript;;

let generic_parse_test = generic_test parse_syntax;;

let test_stat_noop = generic_parse_test
  "stat_noop" "" STAT_NOOP

let test_stat_expr = generic_parse_test
  "stat_expr" "0" (STAT_EXPR(EXPR_LITERAL(LITERAL_INT(0))))

let test_stat_assign = generic_parse_test
  "stat_assign" "a = 0 in a" (STAT_ASSIGN([EXPR_IDENTIFIER("a")], [EXPR_LITERAL(LITERAL_INT(0))], STAT_EXPR(EXPR_IDENTIFIER("a"))))

let test_stat_dropvalue = generic_parse_test
  "stat_dropvalue" "0; 1" (STAT_DROPVALUE(EXPR_LITERAL(LITERAL_INT 0), STAT_EXPR(EXPR_LITERAL(LITERAL_INT 1))))

let test_stat_dropvalues = generic_parse_test
  "stat_dropvalues" "0; 1; 2" (STAT_DROPVALUE(EXPR_LITERAL(LITERAL_INT 0), STAT_DROPVALUE(EXPR_LITERAL(LITERAL_INT 1), STAT_EXPR(EXPR_LITERAL(LITERAL_INT 2)))))

let () = run_tests_and_display "PARSING" [
  test_stat_noop;
  test_stat_expr;
  test_stat_assign;
  test_stat_dropvalue;
  test_stat_dropvalues;
]
