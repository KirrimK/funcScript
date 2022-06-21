(* FSsyntax.ml *)

(* Unary operations *)
type un_op = 
| Uminus
| Not

(* Binary operations *)
type bin_op = 
| Add
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

let unop_str = fun o ->
  match o with
  | Not -> "Not"
  | Uminus -> "Uminus"

let binop_str = fun o ->
  match o with
  | Add -> "Add"
  | Sub -> "Sub"
  | Mult -> "Mult"
  | Div -> "Div"
  | Mod -> "Mod"
  | Same -> "Same"
  | Different -> "Different"
  | Lt -> "Lt"
  | Gt -> "Gt"
  | Let -> "Let"
  | Get -> "Get"

type stat =
  | STAT_NOOP
  | STAT_EXPR of expr
  | STAT_ASSIGN of expr list * expr list * stat
  | STAT_MATCH of expr * (expr * expr * stat) list
  | STAT_PAUSE of expr * stat

and expr =
  | EXPR_LITERAL of lit
  | EXPR_UNARY of un_op * expr
  | EXPR_BINARY of bin_op * expr * expr
  | EXPR_FCALL of expr * expr list

and lit =
  | LITERAL_ANY
  | LITERAL_INT of int
  | LITERAL_FLOAT of float
  | LITERAL_STRING of string
  | LITERAL_BOOL of bool
  | LITERAL_LIST of lit list
  | LITERAL_FUNCTION of string list * stat
  | LITERAL_COROUTINE of string list * stat
  | LITERAL_NONE

let rec stat_str = fun st ->
  match st with
  | STAT_NOOP -> "NOOP"
  | STAT_PAUSE (e, stn) -> Printf.sprintf "PAUSE (%s); %s" (expr_str e) (stat_str stn)
  | STAT_ASSIGN (varls, vlls, stn) -> Printf.sprintf "ASSIGN (%s = %s); %s" (String.concat ", " (List.map expr_str varls)) (String.concat ", " (List.map expr_str vlls)) (stat_str stn)
  | STAT_MATCH (_, _)  -> Printf.sprintf "MATCH"
  | STAT_EXPR e -> Printf.sprintf "EXPR %s" (expr_str e)

and expr_str = fun ex ->
  match ex with
  | EXPR_LITERAL l -> Printf.sprintf "LITERAL %s" (lit_str l)
  | EXPR_UNARY (uo, e) -> Printf.sprintf "UNARY (%s %s)" (unop_str uo) (expr_str e)
  | EXPR_BINARY (bo, ea, eb) -> Printf.sprintf "BINARY (%s %s %s)" (expr_str ea) (binop_str bo) (expr_str eb)
  | EXPR_FCALL (ef, argls) -> Printf.sprintf "FCALL (%s(%s))" (expr_str ef) (String.concat ", " (List.map expr_str argls))

and lit_str = fun l ->
  match l with
  | LITERAL_ANY -> "ANY"
  | LITERAL_NONE -> "NONE"
  | LITERAL_INT i -> Printf.sprintf "INT %d" i
  | LITERAL_BOOL b -> Printf.sprintf "BOOL %b" b
  | LITERAL_FLOAT f -> Printf.sprintf "FLOAT %f" f
  | LITERAL_STRING s -> Printf.sprintf "STRING %s" s
  | LITERAL_LIST ll -> Printf.sprintf "LIST [%s]" (String.concat ", " (List.map lit_str ll))
  | LITERAL_FUNCTION (args, stf) -> Printf.sprintf "(%s -> %s)" (String.concat ", " args) (stat_str stf)
  | LITERAL_COROUTINE (args, stf) -> Printf.sprintf "(%s @> %s)" (String.concat ", " args) (stat_str stf)
