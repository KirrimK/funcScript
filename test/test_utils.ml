(* Test_utils.ml *)

let run_tests_and_display = fun test_type_str test_list ->
  Printf.printf "Running tests [%s]\n" test_type_str;
  let total_tests = List.length test_list in
  let res_list = List.map (fun x -> x ()) test_list in
  let ok_count = ref 0 in
  ignore (List.map (fun x -> let (test_name, res) = x in
                              begin match res with
                                Ok(b) -> if b then
                                            ok_count := !ok_count + 1
                                          else Printf.printf "[%s %s] failed\n" test_type_str test_name
                              | Error(msg) -> Printf.printf "[%s %s] failed with error: %s\n" test_type_str test_name msg end) res_list);
  Printf.printf "%d/%d [%s] tests have passed" !ok_count total_tests test_type_str
