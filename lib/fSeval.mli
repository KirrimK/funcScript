val eval_stat :
  FSobjs.eval_context ->
  FSsyntax.stat -> FSobjs.eval_context * FSobjs.eval_obj
val eval_expr : FSobjs.eval_context -> FSsyntax.expr -> FSobjs.eval_obj
val destructure_assign :
  FSobjs.eval_context -> FSsyntax.expr list -> FSobjs.eval_obj list -> unit
val is_truthy : FSobjs.eval_obj -> bool
val syntax_lit_to_obj :
  FSobjs.eval_context -> FSsyntax.lit -> FSobjs.eval_obj
val unary_op : FSsyntax.un_op -> FSobjs.eval_obj -> FSobjs.eval_obj
val binary_op :
  FSsyntax.bin_op -> FSobjs.eval_obj -> FSobjs.eval_obj -> FSobjs.eval_obj
val functioncall :
  FSobjs.eval_context ->
  FSsyntax.expr -> FSsyntax.expr list -> FSobjs.eval_obj
