val next_unclear_id : unit -> int
type type_obj =
    Any_t
  | None_t
  | Int_t
  | Float_t
  | String_t
  | Bool_t
  | List_t of type_obj
  | Function_t of type_obj list * type_obj
  | Unclear_t of int
val type_obj_str : type_obj -> string
