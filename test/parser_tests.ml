(* Parser_tests.ml*)

open Test_utils;;
open FuncScript;;
let generic_parse_test = fun test_name to_parse expected ()->
  try
    let parsed = parse_syntax to_parse in
    let is_as_expected = (parsed = expected) in
    (test_name, Ok(is_as_expected))
  with e ->
    (test_name, Error(Printexc.to_string e))

let test_stat_noop = generic_parse_test
  "stat_noop" "" STAT_NOOP

let test_stat_pause = generic_parse_test
  "stat_pause" "pause 0; #" (STAT_PAUSE(EXPR_LITERAL(LITERAL_INT(0)), STAT_EXPR(EXPR_LITERAL(LITERAL_NONE))))

let test_stat_expr = generic_parse_test
  "stat_expr" "0" (STAT_EXPR(EXPR_LITERAL(LITERAL_INT(0))))

  let test_stat_assign = generic_parse_test
  "stat_assign" "a = 0 in a" (STAT_ASSIGN([EXPR_IDENTIFIER("a")], [EXPR_LITERAL(LITERAL_INT(0))], STAT_EXPR(EXPR_IDENTIFIER("a"))))

let () = run_tests_and_display "PARSING" [
  test_stat_noop;
  test_stat_pause;
  test_stat_expr;
  test_stat_assign;
]
