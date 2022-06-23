val parse_syntax : string -> FSsyntax.stat
val syntax_to_string : FSsyntax.stat -> string
val new_std_context: unit -> FSeval.eval_context
val copy_context : FSeval.eval_context -> FSeval.eval_context
val eval_syntax : FSeval.eval_context -> FSsyntax.stat -> FSeval.eval_context * FSeval.eval_obj
val obj_to_string : FSeval.eval_obj -> string