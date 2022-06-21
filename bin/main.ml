open FuncScript;;

let rec read_stdin = fun acc ->
  try
    read_stdin (String.concat "" [acc; (really_input_string stdin 1)])
  with _ -> acc;;

let () = 
  Printf.printf "(Heredoc) > %!";
  let input = read_stdin "" in
  let synt = parse_syntax input in
  Printf.printf "%s" (syntax_to_string synt)