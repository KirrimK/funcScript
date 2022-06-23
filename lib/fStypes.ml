(* FStypes.ml *)

type type_obj = 
  | None_t
  | Int_t
  | Float_t
  | String_t
  | Bool_t
  | List_t of type_obj
  | Function_t of type_obj list * type_obj
  | Coroutine_t of type_obj list * type_obj

let rec type_obj_str = fun tp ->
  match tp with
  | None_t -> "None_t"
  | Int_t -> "Int_t"
  | Float_t -> "Float_t"
  | List_t a -> Printf.sprintf "(%s List_t)" (type_obj_str a)
  | Bool_t -> "Bool_t"
  | String_t -> "String_t"
  | Function_t (tpins, tpout) -> Printf.sprintf "(%s -> %s)" (String.concat ", " (List.map type_obj_str tpins)) (type_obj_str tpout)
  | Coroutine_t (tpins, tpout) -> Printf.sprintf "(%s @> %s)" (String.concat ", " (List.map type_obj_str tpins)) (type_obj_str tpout)
