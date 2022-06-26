(* FStypes.ml *)

type type_obj = 
  | Any_t
  | None_t
  | Int_t
  | Float_t
  | String_t
  | Bool_t
  | List_t
  | Function_t of type_obj list * type_obj

let rec type_obj_str = fun tp ->
  match tp with
  | Any_t -> "Any_t"
  | None_t -> "None_t"
  | Int_t -> "Int_t"
  | Float_t -> "Float_t"
  | List_t -> "List_t"
  | Bool_t -> "Bool_t"
  | String_t -> "String_t"
  | Function_t (tpins, tpout) -> Printf.sprintf "(%s -> %s)" (String.concat ", " (List.map type_obj_str tpins)) (type_obj_str tpout)
