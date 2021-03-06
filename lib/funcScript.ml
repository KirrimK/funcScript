(* FuncScript.ml *)
open FSsyntax;;
open FSeval;;
open FSstdlib;;
open FStyping;;
open FStypes;;
open FSobjs;;

let parse_syntax = fun st ->
  let lexbuf = Lexing.from_string (String.concat "" [st; "\000"]) in
  FSparser.main FSlexer.token lexbuf

let syntax_to_string = stat_str

let new_std_context = fun () -> copy_context std_context

let context_to_type_context = convert_context_to_type_context

let copy_context = copy_context

let eval_syntax = eval_stat

let type_check_syntax = type_desc_stat

let obj_to_string = eval_obj_str

let type_of_obj = get_type_of_obj

let type_to_string = type_obj_str

let reset_unclear_ids = FStypes.reset_unclear_id;;

let set_typing_debug = fun b -> FStyping.debug_typing := b;;