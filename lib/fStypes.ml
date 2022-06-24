(* FStypes.ml *)

let unclear_id = ref 0

let next_unclear_id = fun () ->
  let id = !unclear_id in
  unclear_id := !unclear_id + 1;
  id

type type_obj = 
  | Any_t
  | None_t
  | Int_t
  | Float_t
  | String_t
  | Bool_t
  | List_t of type_obj
  | Function_t of type_obj list * type_obj
  | Coroutine_t of type_obj list * type_obj
  | Unclear_t of (type_obj, int) result

let rec type_obj_str = fun tp ->
  match tp with
  | Any_t -> "Any_t"
  | None_t -> "None_t"
  | Int_t -> "Int_t"
  | Float_t -> "Float_t"
  | List_t a -> Printf.sprintf "(%s List_t)" (type_obj_str a)
  | Bool_t -> "Bool_t"
  | String_t -> "String_t"
  | Function_t (tpins, tpout) -> Printf.sprintf "(%s -> %s)" (String.concat ", " (List.map type_obj_str tpins)) (type_obj_str tpout)
  | Coroutine_t (tpins, tpout) -> Printf.sprintf "(%s @> %s)" (String.concat ", " (List.map type_obj_str tpins)) (type_obj_str tpout)
  | Unclear_t r -> Printf.sprintf "(Unclear_t %s)" (begin match r with Ok t -> (type_obj_str t) | Error i -> string_of_int i end)
