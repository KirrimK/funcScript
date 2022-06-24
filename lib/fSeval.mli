type eval_context = { mutable variables : (string, eval_obj) Hashtbl.t; }
and eval_obj =
    Eval_None
  | Eval_None_Toplevel
  | Eval_Int of int
  | Eval_Float of float
  | Eval_String of string
  | Eval_Bool of bool
  | Eval_List of eval_obj list
  | Eval_Function of string list * eval_obj list * FSsyntax.stat
  | Eval_Coroutine of string list * eval_obj list * FSsyntax.stat list ref *
      eval_context
  | Eval_OCaml_Function of eval_obj list *
      (eval_context -> eval_obj list -> eval_obj) * FStypes.type_obj list *
      FStypes.type_obj
val eval_obj_str : eval_obj -> string
val copy_context : eval_context -> eval_context
val eval_stat : eval_context -> FSsyntax.stat -> eval_context * eval_obj
val eval_expr : eval_context -> FSsyntax.expr -> eval_obj
val destructure_assign :
  eval_context -> FSsyntax.expr list -> eval_obj list -> unit
val is_truthy : eval_obj -> bool
val syntax_lit_to_obj : eval_context -> FSsyntax.lit -> eval_obj
val unary_op : FSsyntax.un_op -> eval_obj -> eval_obj
val binary_op : FSsyntax.bin_op -> eval_obj -> eval_obj -> eval_obj
val functioncall :
  eval_context -> FSsyntax.expr -> FSsyntax.expr list -> eval_obj
