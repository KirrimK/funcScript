val debug_typing : bool ref
val first_elements_of_list : 'a list -> int -> 'a list
val last_elements_of_list : 'a list -> int -> 'a list
type type_context = {
  mutable type_variables : (string, FStypes.type_obj) Hashtbl.t;
}
val copy_tcontext : type_context -> type_context
val get_type_of_obj : FSobjs.eval_obj -> FStypes.type_obj
val convert_context_to_type_context : FSobjs.eval_context -> type_context
val type_check_args : FSobjs.eval_obj list -> FStypes.type_obj list -> bool
val is_unclear_list : FStypes.type_obj -> bool
val is_list : FStypes.type_obj -> bool
val type_desc_unop : FSsyntax.un_op -> FStypes.type_obj -> FStypes.type_obj
val type_asc_unop : FSsyntax.un_op -> FStypes.type_obj -> FStypes.type_obj
val type_desc_binop :
  FSsyntax.bin_op -> FStypes.type_obj -> FStypes.type_obj -> FStypes.type_obj
val type_asc_binop :
  FSsyntax.bin_op -> FStypes.type_obj -> FStypes.type_obj * FStypes.type_obj
val index_of : 'a -> 'a list -> int
val type_desc_fcall :
  FStypes.type_obj -> FStypes.type_obj list -> FStypes.type_obj
val type_desc_stat : type_context -> FSsyntax.stat -> FStypes.type_obj
val type_asc_stat : type_context -> FSsyntax.stat -> FStypes.type_obj -> unit
val type_asc_expr :
  type_context -> FSsyntax.expr -> FStypes.type_obj -> FStypes.type_obj
val type_desc_expr : type_context -> FSsyntax.expr -> FStypes.type_obj
val type_of_list :
  type_context -> FSsyntax.expr list -> FStypes.type_obj -> FStypes.type_obj
val type_desc_literal : type_context -> FSsyntax.lit -> FStypes.type_obj
val type_function :
  type_context -> string list -> FSsyntax.stat -> FStypes.type_obj
val type_assign :
  type_context -> FSsyntax.expr list -> FStypes.type_obj list -> unit
