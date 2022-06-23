open FuncScript;;

let rec read_stdin = fun acc ->
  try
    read_stdin (String.concat "" [acc; (really_input_string stdin 1)])
  with _ -> acc;;

let () = 
  Printf.printf "(Heredoc) > %!";
  let input = read_stdin "" in
  let synt = parse_syntax input in
  (*Printf.printf "%s\n" (syntax_to_string synt);*)
  let (_, vl) = eval_syntax (new_std_context ()) synt in
  Printf.printf "\n%s" (obj_to_string vl)
