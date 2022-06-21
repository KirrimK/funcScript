%{
    open FSsyntax
%}

%token EQUAL
%token IN

%token SEMICOLON

%token LPAR
%token RPAR

%token IF
%token THEN
%token ELSE

%token ANY
%token NONE

%token BEGIN
%token END

%token COMMA
%token PAUSE

%token <string> IDENTIFIER
%token <string> STRINGLIT
%token <int> INTLIT
%token <float> FLOATLIT
%token <bool> BOOLLIT

%token FN
%token CR

%token SAME
%token DIFFERENT

%token LT
%token GT
%token LET
%token GET

%token ADD
%token SUB
%token MULT
%token DIV
%token MOD

%token NOT

%token EOF

(* Token priorities *)

%left SAME DIFFERENT 
%left LT LET GT GET

%left ADD SUB
%left MULT DIV MOD

%right NOT

%left LPAR

%start <FSsyntax.stat> main

%%

main:
  st = stat EOF {st}
| EOF {STAT_NOOP}

stat:
  sst = simple_stat {sst}
| PAUSE ex = expr SEMICOLON st = stat {STAT_PAUSE(ex, st)}
| IF ex_cond = expr THEN if_st = stat ELSE else_st = stat {STAT_IF(ex_cond, if_st, else_st)}

simple_stat:
  ex = expr {STAT_EXPR(ex)}
| exvar = expr_list EQUAL exval = expr_list IN sstn = simple_stat {STAT_ASSIGN(exvar, exval, sstn)}
| BEGIN st = stat END {st}

expr:
  lt = literal {EXPR_LITERAL(lt)}
| id = IDENTIFIER {EXPR_IDENTIFIER(id)}
| uo = un_op ex = expr {EXPR_UNARY(uo, ex)}
| exa = expr bo = bin_op exb = expr {EXPR_BINARY(bo, exa, exb)}
| exf = expr LPAR RPAR {EXPR_FCALL(exf, [])}
| exf = expr LPAR ex_ls = expr_list RPAR {EXPR_FCALL(exf, ex_ls)}

expr_list:
  ex = expr {[ex]}
| ex = expr COMMA ex_ls = expr_list {ex::ex_ls}

literal:
  ANY {LITERAL_ANY}
| NONE {LITERAL_NONE}
| il = INTLIT {LITERAL_INT(il)}
| bl = BOOLLIT {LITERAL_BOOL(bl)}
| fl = FLOATLIT {LITERAL_FLOAT(fl)}
| st = STRINGLIT {LITERAL_STRING(st)}
| LPAR id_ls = id_list FN st = stat RPAR {LITERAL_FUNCTION(id_ls, st)}
| LPAR id_ls = id_list CR st = stat RPAR {LITERAL_COROUTINE(id_ls, st)}


id_list:
  id = IDENTIFIER {[id]}
| id = IDENTIFIER id_ls = id_list {id::id_ls}

%inline un_op:
  NOT {Not}
| SUB {Uminus}

%inline bin_op:
  ADD {Add}
| SUB {Sub}
| MULT {Mult}
| DIV {Div}
| MOD {Mod}
| SAME {Same}
| DIFFERENT {Different}
| LT {Lt}
| GT {Gt}
| LET {Let}
| GET {Get}
