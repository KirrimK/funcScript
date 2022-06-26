type un_op = Uminus | Not
type bin_op =
    Add
  | Sub
  | Mult
  | Div
  | Mod
  | Same
  | Different
  | Lt
  | Gt
  | Let
  | Get
  | Head
val unop_str : un_op -> string
val binop_str : bin_op -> string
type stat =
    STAT_NOOP
  | STAT_EXPR of expr
  | STAT_ASSIGN of expr list * expr list * stat
  | STAT_ASSIGN_TOPLEVEL of expr list * expr list
  | STAT_IF of expr * stat * stat
  | STAT_DROPVALUE of expr * stat
and expr =
    EXPR_LITERAL of lit
  | EXPR_UNARY of un_op * expr
  | EXPR_BINARY of bin_op * expr * expr
  | EXPR_FCALL of expr * expr list
  | EXPR_IDENTIFIER of string
and lit =
  | LITERAL_INT of int
  | LITERAL_FLOAT of float
  | LITERAL_STRING of string
  | LITERAL_BOOL of bool
  | LITERAL_LIST of expr list
  | LITERAL_FUNCTION of string list * stat
  | LITERAL_NONE
val stat_str : stat -> string
val expr_str : expr -> string
val lit_str : lit -> string
