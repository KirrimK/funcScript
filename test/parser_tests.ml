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

let test_one_semicolon = generic_parse_test
  "one_semicolon"
  ";"
  STAT_NOOP

let () = run_tests_and_display "PARSING" [
  test_one_semicolon;
]
