(* FStyping.ml *)

open FStypes;;
open FSobjs;;
open FSsyntax;;

let rec get_type_of_obj = fun obj ->
  match obj with
  | Eval_None
  | Eval_None_Toplevel -> None_t
  | Eval_Int _ -> Int_t
  | Eval_Float _ -> Float_t
  | Eval_String _ -> String_t
  | Eval_Bool _ -> Bool_t
  | Eval_List l -> List_t (get_type_of_obj (List.hd l))
  | Eval_Function (_, _, _, tpins, tpout) -> Function_t (tpins, tpout)
  | Eval_OCaml_Function (_, _, tpins, tpout) -> Function_t (tpins, tpout)

let type_check_args = fun tot_args tpins ->
  true

  let type_function = fun context argnames stf ->
  ([], Any_t)
