(* FSobjs.ml *)

open FSsyntax;;
open FStypes;;

type eval_context = {
  mutable variables: (string, eval_obj) Hashtbl.t
}

and eval_obj =
  | Eval_None
  | Eval_None_Toplevel
  | Eval_Int of int
  | Eval_Float of float
  | Eval_String of string
  | Eval_Bool of bool
  | Eval_List of eval_obj list
  | Eval_Function of string list * eval_obj list * stat * (*typing*) type_obj list * type_obj
  | Eval_OCaml_Function of eval_obj list * (eval_context -> eval_obj list -> eval_obj) * (*typing*) type_obj list * type_obj

let rec eval_obj_str = fun obj ->
  match obj with
  | Eval_None -> "None"
  | Eval_None_Toplevel -> "None_Toplevel"
  | Eval_Int i -> Printf.sprintf "%d" i
  | Eval_Float f -> Printf.sprintf "%f" f
  | Eval_Bool b -> Printf.sprintf "%b" b
  | Eval_String s -> Printf.sprintf "\"%s\""s
  | Eval_List l -> Printf.sprintf "[%s]" (String.concat ", " (List.map eval_obj_str l))
  | Eval_Function(_, _, _, _, _) -> "Function"
  | Eval_OCaml_Function(_, _, _, _) -> "OCaml_Function"

let copy_context = fun context ->
  {
    variables = Hashtbl.copy (context.variables);
  }
