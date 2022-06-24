open FuncScript;;

(*let rec read_stdin = fun acc ->
  try
    read_stdin (String.concat "" [acc; (really_input_string stdin 1)])
  with _ -> acc;;*)

let () =
  Printf.printf "(funcScript repl v0.01 by Rémy B.)\n";
  let ctx = ref (new_std_context ()) in
  let is_done = ref false in
  while not !is_done do
    Printf.printf "# %!";
    let finished_input = ref false in
    let input = ref "" in
    let synt = ref (parse_syntax "") in
    while not !finished_input do
      let temp = read_line() in
      let () = if temp = "flush" then
        begin input := ""; finished_input := true; end
      else
        input := !input ^ (temp) in
      try
        synt := parse_syntax !input;
        finished_input := true;
      with _ ->
        () 
    done;
    (*Printf.printf "%s\n" (syntax_to_string synt);*)
    try
      let (new_ctx, vl) = eval_syntax !ctx !synt in
      ctx := new_ctx;
      if vl <> Eval_None_Toplevel then
        Printf.printf "- :%s\n" (obj_to_string vl);
    with Failure f ->
      Printf.printf "%s\n" (f)
  done
