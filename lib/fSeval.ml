(* FSEval.ml *)

open FSsyntax;;
open FStypes;;
open FSobjs;;
open FStyping;;

let rec eval_stat = fun context stat ->
  match stat with
  | STAT_NOOP -> (context, Eval_None)
  | STAT_EXPR e -> let expr_vl = eval_expr context e in
    (context, expr_vl)
  | STAT_ASSIGN (varls, expr_ls, stn)-> let expr_vl_ls = List.map (eval_expr context) expr_ls in
    destructure_assign context varls expr_vl_ls;
    eval_stat context stn
  | STAT_ASSIGN_TOPLEVEL (varls, expr_ls) -> let expr_vl_ls = List.map (eval_expr context) expr_ls in
    destructure_assign context varls expr_vl_ls;
    (context, Eval_None_Toplevel)
  | STAT_IF (e, sti, ste) -> let e_vl = eval_expr context e in
    if (is_truthy e_vl) = true then
      eval_stat context sti
    else
      eval_stat context ste
  | STAT_DROPVALUE (e, stn) -> let _ = eval_expr context e in
      eval_stat context stn

and eval_expr = fun context expr ->
  match expr with
  | EXPR_LITERAL l -> syntax_lit_to_obj context l
  | EXPR_UNARY (uo, e) -> unary_op uo (eval_expr context e)
  | EXPR_BINARY (bo, ea, eb) -> binary_op bo (eval_expr context ea) (eval_expr context eb)
  | EXPR_FCALL (ef, eal) -> functioncall context ef eal
  | EXPR_IDENTIFIER id -> match Hashtbl.find_opt context.variables id with
    | Some v -> v
    | None -> failwith (Printf.sprintf "(Eval) variable \"%s\" not found, did type-check get bypassed?"  id)

and destructure_assign = fun context varls expr_vl_ls->
  let rec assign_values = fun vars values ->
    match vars, values with
    | [], [] -> ()
    | [], _ -> ()
    | (EXPR_IDENTIFIER i)::tlv, [] -> Hashtbl.add context.variables i Eval_None;
                                      assign_values tlv []
    | (EXPR_LITERAL(LITERAL_LIST l))::tlv, [] -> assign_values l [];
                                                  assign_values tlv []
    | (EXPR_IDENTIFIER i)::tlv, hde::tle -> Hashtbl.add context.variables i hde; assign_values tlv tle
    | (EXPR_BINARY(Head, EXPR_IDENTIFIER hdi, tli))::tlv, Eval_List(hdhde::tlhde)::tle ->
      Hashtbl.add context.variables hdi hdhde;
      destructure_assign context [tli] [Eval_List(tlhde)];
      assign_values tlv tle;
    | (EXPR_LITERAL(LITERAL_NONE))::tlv, _::tle -> assign_values tlv tle
    | (EXPR_LITERAL(LITERAL_LIST l))::tlv, (Eval_List el)::tle -> destructure_assign context l el; assign_values tlv tle
    | a::_, b::_ -> failwith (Printf.sprintf "(Eval) couldn't assign value (%s) to expression (%s)" (eval_obj_str b) (expr_str a))
    | _, _ -> failwith "(Eval) error while assigning values" in 
  assign_values varls expr_vl_ls

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
  | LITERAL_LIST l -> Eval_List (List.map (eval_expr context) l)
  | LITERAL_FUNCTION (argnames, stf) ->
    begin match type_function (convert_context_to_type_context context) argnames stf(*type_function context argnames stf*) with
    | Function_t(tpins, tpout) -> Eval_Function (argnames, [], stf, tpins, tpout)
    | a -> failwith (Printf.sprintf "(Eval) type_function returned a (%s) instead of function" (type_obj_str a)) end
  | LITERAL_NONE -> Eval_None

and unary_op = fun uo vl ->
  match uo, vl with
  | Not, Eval_Bool b -> Eval_Bool (not b)
  | Uminus, Eval_Int i -> Eval_Int (-i)
  | Uminus, Eval_Float f -> Eval_Float (-.f)
  | a, b -> failwith (Printf.sprintf "(Eval) unsupported operation (%s %s), did type-check get bypassed?" (unop_str a) (eval_obj_str b))

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
  | Head, a, Eval_List lsrt when a <> Eval_None -> Eval_List(a::lsrt)
  | a, b, c -> failwith (Printf.sprintf "(Eval) unsupported operation (%s %s %s), did type-check get bypassed?" (eval_obj_str b) (binop_str a) (eval_obj_str c))

and functioncall = fun context ef eal ->
  let evl = eval_expr context ef in
  let vl_ls = List.map (eval_expr context) eal in
  match evl with
  | Eval_Function (argnames, preargs, stf, tpins, tpout) ->
    let tot_args = List.append preargs vl_ls in
    if List.length tot_args < List.length argnames && (type_check_args tot_args tpins) then
      Eval_Function (argnames, tot_args, stf, tpins, tpout)
    else
      (* should have type-checked *)
      let new_context = copy_context context in
      let () = List.iter2 (fun id vl -> Hashtbl.add new_context.variables id vl) argnames tot_args in
      let (_, res) = eval_stat new_context stf in res
  | Eval_OCaml_Function (preargs, func, tpins, tpout) ->
    let tot_args = List.append preargs vl_ls in
    if List.length tot_args < List.length tpins && (type_check_args tot_args tpins) then
      Eval_OCaml_Function (tot_args, func, tpins, tpout)
    else
      (* should have type-checked *)
      let new_context = copy_context context in
      func new_context tot_args
  | _ -> failwith "tried to call a non-callable"