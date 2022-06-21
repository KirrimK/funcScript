{
    open FSparser
    exception Error of string
}

let digit = ['0'-'9']
let int = digit+
let float = (digit* '.' digit+ | digit+ '.' digit*)

let digit = ['0'-'9']
let lower = ['a'-'z']
let upper = ['A'-'Z']
let letter = lower | upper
let other = ['_']
let name = (letter | other) (letter | digit | other)*

rule token = parse
  "true" { BOOLLIT true }
| "false" { BOOLLIT false }
| "pause" {PAUSE}
| '=' { EQUAL }
| '(' {LPAR}
| ')' {RPAR}
| "bg" {BEGIN}
| "nd" {END}
| ';' {SEMICOLON}
| '?' {QUESTION}
| ':' {COLONS}
| '|' {VERTICAL}
| "if" {IF}
| '_' {ANYTHING}
| "->" {FN}
| "@>" {CR}
| '+' {ADD}
| '-' {SUB}
| '*' {MULT}
| '/' {DIV}
| '%' {MOD}
| '!' {NOT}
| "==" {SAME}
| "!=" {DIFFERENT}
| '<' {LT}
| '>' {GT}
| "<=" {LET}
| ">=" {GET}
| ',' {COMMA}
| "//"[^'\r' '\n']* {token lexbuf} (* Comments *)
| [' ' '\t' '\n']+ {token lexbuf} 
| '"' ([^'"']* as st) '"' {STRINGLIT st}
| int as nb {INTLIT (int_of_string nb)}
| float as ft {FLOATLIT (float_of_string ft)}
| name as s { IDENTIFIER s }
| eof | '\000' { EOF }
| _ as nimp { raise (Error (Printf.sprintf "At offset %d: unexpected character: '%c'" (Lexing.lexeme_start lexbuf) nimp)) }