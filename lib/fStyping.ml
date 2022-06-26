(* FStyping.ml *)

open FStypes;;
open FSeval;;

let get_type_of_obj = fun obj ->
  match obj with
  | Eval_None
  | Eval_None_Toplevel -> None_t
  | Eval_Int _ -> Int_t
  | Eval_Float _ -> Float_t
  | Eval_String _ -> String_t
  | Eval_Bool _ -> Bool_t
  | Eval_List _ -> List_t
  | Eval_Function (argnames, _, _) -> Function_t (List.map (fun _ -> Any_t) argnames, Any_t)
  | Eval_OCaml_Function (_, _, tpins, tpout) -> Function_t (tpins, tpout)
