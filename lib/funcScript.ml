(* FuncScript.ml *)
open FSsyntax;;
let parse_syntax = fun st ->
  let lexbuf = Lexing.from_string (String.concat "" [st; "\000"]) in
  FSparser.main FSlexer.token lexbuf

let syntax_to_string = stat_str