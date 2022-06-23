(* FSEval.ml *)

open FSsyntax;;
open FStypes;;

type eval_context = {
  mutable variables: (string, eval_obj) Hashtbl.t
}

and eval_obj =
  | Eval_None
  | Eval_Int of int
  | Eval_Float of float
  | Eval_String of string
  | Eval_Bool of bool
  | Eval_List of eval_obj list
  | Eval_Function of string list * eval_obj list * stat
  | Eval_Coroutine of string list * eval_obj list * stat * eval_context
  | Eval_OCaml_Function of eval_obj list * (eval_obj list -> eval_obj) * (*typing*) type_obj list * type_obj
  | Eval_OCaml_Coroutine of eval_obj list * (eval_obj list -> eval_obj) *(*typing*) type_obj list * type_obj

let rec eval_obj_str = fun obj ->
  match obj with
  | Eval_None -> "None"
  | Eval_Int i -> Printf.sprintf "%d" i
  | Eval_Float f -> Printf.sprintf "%f" f
  | Eval_Bool b -> Printf.sprintf "%b" b
  | Eval_String s -> Printf.sprintf "\"%s\""s
  | Eval_List l -> Printf.sprintf "[%s]" (String.concat ", " (List.map eval_obj_str l))
  | Eval_Function(_, _, _) -> "Function"
  | Eval_Coroutine(_, _, _, _) -> "Coroutine"
  | Eval_OCaml_Function(_, _, tpins, tpout) -> Printf.sprintf "(OCaml %s)" (type_obj_str (Function_t (tpins, tpout)))
  | Eval_OCaml_Coroutine(_, _, tpins, tpout) -> Printf.sprintf "(OCaml %s)" (type_obj_str (Coroutine_t (tpins, tpout)))

let copy_context = fun context ->
  {variables = Hashtbl.copy (context.variables)}

let rec eval_stat = fun context stat ->
  match stat with
  | STAT_NOOP -> (context, Eval_None)
  | STAT_EXPR e -> let expr_vl = eval_expr context e in
    (context, expr_vl)
  | STAT_ASSIGN (varls, expr_ls, stn)-> let expr_vl_ls = List.map (eval_expr context) expr_ls in
    destructure_assign context varls expr_vl_ls;
    eval_stat context stn
  | STAT_IF (e, sti, ste) -> let e_vl = eval_expr context e in
    if (is_truthy e_vl) = true then
      eval_stat context sti
    else
      eval_stat context ste
  | STAT_PAUSE ((*s, stn*)_, _) -> failwith "unsupported"
  | STAT_DROPVALUE (e, stn) -> let _ = eval_expr context e in
      eval_stat context stn

and eval_expr = fun context expr ->
  match expr with
  | EXPR_LITERAL l -> syntax_lit_to_obj context l
  | EXPR_UNARY (uo, e) -> unary_op uo (eval_expr context e)
  | EXPR_BINARY (bo, ea, eb) -> binary_op bo (eval_expr context ea) (eval_expr context eb)
  | EXPR_FCALL ((*ef, eal*)_, _) -> failwith "unsupported"
  | EXPR_IDENTIFIER id -> match Hashtbl.find_opt context.variables id with
    | Some v -> v
    | None -> failwith (Printf.sprintf "Error while evaluating [%s]: variable '%s' not found" (expr_str expr) id)

and destructure_assign = fun (*context varls expr_vl_ls *)_ _ _->
  ()

and is_truthy = fun obj ->
  match obj with
  | Eval_None -> false
  | Eval_Int i -> i <> 0
  | Eval_Float f -> f <> 0.0
  | Eval_Bool b -> b
  | _ -> true

and syntax_lit_to_obj = fun context l ->
  match l with
  | LITERAL_INT i -> Eval_Int i
  | LITERAL_FLOAT f -> Eval_Float f
  | LITERAL_BOOL b -> Eval_Bool b
  | LITERAL_STRING s -> Eval_String s
  | LITERAL_LIST _ -> failwith "unsupported"
  | LITERAL_FUNCTION (argnames, stf) -> Eval_Function (argnames,[], stf)
  | LITERAL_COROUTINE (argnames, stf) -> Eval_Coroutine (argnames,[], stf, copy_context context)
  | LITERAL_NONE -> Eval_None

and unary_op = fun uo vl ->
  match uo, vl with
  | Not, Eval_Bool b -> Eval_Bool (not b)
  | Uminus, Eval_Int i -> Eval_Int (-i)
  | Uminus, Eval_Float f -> Eval_Float (-.f)
  | _, _ -> failwith "unsupported"

and binary_op = fun bo el er ->
  match bo, el, er with
  | Add, Eval_Bool bl, Eval_Bool br -> Eval_Bool (bl || br)
  | Add, Eval_Int il, Eval_Int ir -> Eval_Int (il + ir)
  | Add, Eval_Float fl, Eval_Float fr -> Eval_Float (fl +. fr)
  | Add, Eval_String stl, Eval_String str -> Eval_String (stl ^ str)
  | Add, Eval_List lslf, Eval_List lsrt -> Eval_List (List.append lslf lsrt)
  | Sub, Eval_Int il, Eval_Int ir -> Eval_Int (il - ir)
  | Sub, Eval_Float fl, Eval_Float fr -> Eval_Float (fl -. fr)
  | Sub, Eval_List lslf, Eval_List lsrt -> 
    let rec remove_if_present = fun acc ls ->
      match ls with
      | [] -> acc
      | hd::tl -> let new_acc = List.filter (fun x -> x <> hd) acc in
                  remove_if_present new_acc tl
    in Eval_List (remove_if_present lslf lsrt)
  | Mult, Eval_Bool bl, Eval_Bool br -> Eval_Bool (bl && br)
  | Mult, Eval_Int il, Eval_Int ir -> Eval_Int (il*ir)
  | Mult, Eval_Float fl, Eval_Float fr -> Eval_Float (fl*.fr)
  | Mult, Eval_String stl, Eval_Int ir -> 
    let rec repeat n s =
      if n = 0 then
        ""
      else s ^ repeat (n - 1) s
    in Eval_String (repeat ir stl)
  | Mult, Eval_List lslf, Eval_Int ir ->
    let rec repeat n ls =
      if n = 0 then
        []
      else List.append ls (repeat (n - 1) ls)
    in Eval_List (repeat ir lslf)
  | Div, Eval_Int il, Eval_Int ir -> Eval_Int(il/ir)
  | Div, Eval_Float fl, Eval_Float fr -> Eval_Float(fl/.fr)
  | Mod, Eval_Int il, Eval_Int ir -> Eval_Int(il mod ir)
  | Mod, Eval_Float fl, Eval_Float fr -> Eval_Float(mod_float fl fr)
  | Same, a, b -> Eval_Bool(a = b)
  | Different, a, b -> Eval_Bool(a <> b)
  | Lt, Eval_Int il, Eval_Int ir -> Eval_Bool(il < ir)
  | Gt, Eval_Int il, Eval_Int ir -> Eval_Bool(il > ir)
  | Let, Eval_Int il, Eval_Int ir -> Eval_Bool(il <= ir)
  | Get, Eval_Int il, Eval_Int ir -> Eval_Bool(il >= ir)
  | Lt, Eval_Float fl, Eval_Float fr -> Eval_Bool((compare fl fr)<1)
  | Gt, Eval_Float fl, Eval_Float fr -> Eval_Bool((compare fl fr)>1)
  | Let, Eval_Float fl, Eval_Float fr -> Eval_Bool((compare fl fr)<1||(compare fl fr)=0)
  | Get, Eval_Float fl, Eval_Float fr -> Eval_Bool((compare fl fr)>1||(compare fl fr)=0)
  | _, _, _ -> failwith "unsupported"
