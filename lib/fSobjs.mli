type eval_context = { mutable variables : (string, eval_obj) Hashtbl.t; }
and eval_obj =
    Eval_None
  | Eval_None_Toplevel
  | Eval_Int of int
  | Eval_Float of float
  | Eval_String of string
  | Eval_Bool of bool
  | Eval_List of eval_obj list
  | Eval_Function of string list * eval_obj list * FSsyntax.stat *
      FStypes.type_obj list * FStypes.type_obj
  | Eval_OCaml_Function of eval_obj list *
      (eval_context -> eval_obj list -> eval_obj) * FStypes.type_obj list *
      FStypes.type_obj
val eval_obj_str : eval_obj -> string
val copy_context : eval_context -> eval_context
