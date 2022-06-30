val get_type_of_obj : FSobjs.eval_obj -> FStypes.type_obj
val type_check_args : FSobjs.eval_obj list -> FStypes.type_obj list -> bool
val type_function : FSobjs.eval_context -> string list -> FSsyntax.stat -> FStypes.type_obj list * FStypes.type_obj
