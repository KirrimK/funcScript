val parse_syntax : string -> FSsyntax.stat
val syntax_to_string : FSsyntax.stat -> string
val new_std_context : unit -> FSobjs.eval_context
val copy_context : FSobjs.eval_context -> FSobjs.eval_context
val eval_syntax :
  FSobjs.eval_context ->
  FSsyntax.stat -> FSobjs.eval_context * FSobjs.eval_obj
val obj_to_string : FSobjs.eval_obj -> string
val type_of_obj : FSobjs.eval_obj -> FStypes.type_obj
val type_to_string : FStypes.type_obj -> string
val type_check_syntax : FStyping.type_context -> FSsyntax.stat -> FStypes.type_obj
val context_to_type_context : FSobjs.eval_context -> FStyping.type_context