(* FStyping.ml *)

open FStypes;;
open FSobjs;;
open FSsyntax;;

(* TODO: recursive functions not working *)

let debug_typing = ref false;;
let debug_print_indent = ref 0;;

let rec first_elements_of_list = fun ls n ->
  if !debug_typing then
    Printf.printf "%sfirst_elements_of_list len(ls):%d n:%d\n" (String.make !debug_print_indent '\t') (List.length ls) n;
  match ls with
  | [] -> []
  | hd::tl -> if n <= 1 then [hd]
              else hd::(first_elements_of_list tl (n - 1));;

let last_elements_of_list = fun ls n ->
  if !debug_typing then
    Printf.printf "%slast_elements_of_list len(ls):%d n:%d\n"  (String.make !debug_print_indent '\t') (List.length ls) n;
    incr debug_print_indent;
  let res = first_elements_of_list (List.rev ls) (List.length ls - n) in
  decr debug_print_indent;
  res

type type_context = {
  mutable type_variables: (string, type_obj) Hashtbl.t
}

let copy_tcontext = fun tctx ->
  if !debug_typing then
    Printf.printf "copy_tcontext\n";
  {type_variables = Hashtbl.copy (tctx.type_variables)}

let rec get_type_of_obj = fun obj ->
  if !debug_typing then
    Printf.printf "%sget_type_of_obj obj:%s\n"  (String.make !debug_print_indent '\t') (eval_obj_str obj);
    incr debug_print_indent;
  match obj with
  | Eval_None
  | Eval_None_Toplevel -> None_t
  | Eval_Int _ -> Int_t
  | Eval_Float _ -> Float_t
  | Eval_String _ -> String_t
  | Eval_Bool _ -> Bool_t
  | Eval_List l -> List_t (get_type_of_obj (List.hd l))
  | Eval_Function (_, objs, _, tpins, tpout) ->
      let remts = last_elements_of_list tpins (List.length objs -1) in
      Function_t (remts, tpout)
  | Eval_OCaml_Function (objs, _, tpins, tpout) ->
      let remts = last_elements_of_list tpins (List.length objs -1) in
      Function_t (remts, tpout)

let convert_context_to_type_context = fun ctx ->
  let old_debug = !debug_typing in
  if old_debug then
    Printf.printf "%sconvert_context_to_type_context\n" (String.make !debug_print_indent '\t');
    debug_typing := false;
  let tctx = {type_variables = Hashtbl.create (Hashtbl.length ctx.variables)} in
  Hashtbl.iter (fun id vl -> Hashtbl.add (tctx.type_variables) id (get_type_of_obj vl)) ctx.variables;
  debug_typing := old_debug;
  tctx

let type_check_args = fun tot_args tpins ->
  if !debug_typing then
    Printf.printf "%stype_check_args tot_args:[%s] tpins:[%s]\n"
    (String.make !debug_print_indent '\t')
    (String.concat "; " (List.map eval_obj_str tot_args))
    (String.concat "; " (List.map type_obj_str tpins));
  let rec local = fun args ins ->
    match args, ins with
    | [], _ -> true
    | _, [] -> false
    | _::tl, (Unclear_t _)::tlt
    | _::tl, Any_t::tlt -> local tl tlt
    | hd::tl, hdt::tlt ->
      if (get_type_of_obj hd) = hdt then
        local tl tlt
      else
        false in
  local tot_args tpins


let rec is_unclear_list = fun tl ->
  if !debug_typing then
    Printf.printf "%sis_unclear_list tl:%s\n"  (String.make !debug_print_indent '\t') (type_obj_str tl);
    incr debug_print_indent;
  match tl with
  | List_t (t) -> is_unclear_list t
  | Unclear_t _ -> decr debug_print_indent; true
  | _ -> decr debug_print_indent; false

let is_list = fun tl ->
  if !debug_typing then
    Printf.printf "%sis_list tl:%s\n"  (String.make !debug_print_indent '\t') (type_obj_str tl);
  match tl with
  | List_t _ -> true
  | _ -> false

let type_desc_unop = fun op t ->
  if !debug_typing then
    Printf.printf "%stype_desc_unop op:%s t:%s\n" (String.make !debug_print_indent '\t') (unop_str op) (type_obj_str t);
  match op,t with
  | Not, Bool_t -> Bool_t
  | Not, Unclear_t _ -> Bool_t
  | Uminus, Int_t -> Int_t
  | Uminus, Float_t -> Float_t
  | Uminus, Unclear_t i -> Unclear_t i
  | a, b -> failwith (Printf.sprintf "(Typing) unary operator (%s) used with argument of incorrect type (%s)" (unop_str a) (type_obj_str b))

let type_asc_unop = fun op tr ->
  if !debug_typing then
    Printf.printf "%stype_asc_unop op:%s tr:%s\n"  (String.make !debug_print_indent '\t') (unop_str op) (type_obj_str tr);
  match op, tr with
  | Not, Bool_t -> Bool_t
  | Not, Unclear_t _ -> Bool_t
  | Uminus, Int_t -> Int_t
  | Uminus, Float_t -> Float_t
  | Uminus, Unclear_t i -> Unclear_t i
  | a, b -> failwith (Printf.sprintf "(Typing) unary operator (%s) was inferred incorrect output of type (%s)" (unop_str a) (type_obj_str b))

let type_desc_binop = fun op ta tb ->
  if !debug_typing then
    Printf.printf "%stype_desc_binop op:%s ta:%s tb:%s\n" (String.make !debug_print_indent '\t') (binop_str op) (type_obj_str ta) (type_obj_str tb);
  match op, ta, tb with
  | Add, Int_t, Int_t -> Int_t
  | Add, Float_t, Float_t -> Float_t
  | Add, String_t, String_t -> String_t
  | Add, Bool_t, Bool_t -> Bool_t
  | Add, List_t t, List_t u when t = u -> List_t t
  | Add, Unclear_t i, Unclear_t _ -> Unclear_t i
  | Add, Int_t, Unclear_t _ -> Int_t
  | Add, Unclear_t _, Int_t -> Int_t
  | Add, Float_t, Unclear_t _ -> Float_t
  | Add, Unclear_t _, Float_t -> Float_t
  | Add, String_t, Unclear_t _ -> String_t
  | Add, Unclear_t _, String_t -> String_t
  | Add, Bool_t, Unclear_t _ -> Bool_t
  | Add, Unclear_t _, Bool_t -> Bool_t
  | Add, List_t t, Unclear_t _ -> List_t t
  | Add, Unclear_t _, List_t t -> List_t t

  | Sub, Int_t, Int_t -> Int_t
  | Sub, Float_t, Float_t -> Float_t
  | Sub, List_t t, List_t u when t = u -> List_t t
  | Sub, Unclear_t i, Unclear_t _-> Unclear_t i
  | Sub, Int_t, Unclear_t _ -> Int_t
  | Sub, Unclear_t _, Int_t -> Int_t
  | Sub, Float_t, Unclear_t _ -> Float_t
  | Sub, Unclear_t _, Float_t -> Float_t
  | Sub, List_t t, Unclear_t _ -> List_t t
  | Sub, Unclear_t _, List_t t -> List_t t

  | Mult, Int_t, Int_t -> Int_t
  | Mult, Float_t, Float_t -> Float_t
  | Mult, Bool_t, Bool_t -> Bool_t
  | Mult, String_t, Int_t -> String_t
  | Mult, List_t t, Int_t -> List_t t
  | Mult, Unclear_t i, Unclear_t _ -> Unclear_t i
  | Mult, Unclear_t i, Int_t -> Unclear_t i
  | Mult, Int_t, Unclear_t _ -> Int_t
  | Mult, Float_t, Unclear_t _ -> Float_t
  | Mult, Bool_t, Unclear_t _ -> Bool_t

  | Div, Int_t, Int_t -> Int_t
  | Div, Float_t, Float_t -> Float_t
  | Div, Unclear_t i, Unclear_t _ -> Unclear_t i
  | Div, Unclear_t _, Int_t -> Int_t
  | Div, Int_t, Unclear_t _ -> Int_t
  | Div, Unclear_t _, Float_t -> Float_t
  | Div, Float_t, Unclear_t _ -> Float_t

  | Mod, Int_t, Int_t -> Int_t
  | Mod, Unclear_t i, Unclear_t _ -> Unclear_t i
  | Mod, Unclear_t _, Int_t -> Int_t
  | Mod, Int_t, Unclear_t _ -> Int_t
  | Mod, Unclear_t _, Float_t -> Float_t
  | Mod, Float_t, Unclear_t _ -> Float_t

  | Same, _, _ -> Bool_t

  | Different, _, _ -> Bool_t

  | (Lt|Let|Gt|Get), Int_t, Int_t -> Bool_t
  | (Lt|Let|Gt|Get), Float_t, Float_t -> Bool_t
  | (Lt|Let|Gt|Get), Unclear_t _, Unclear_t _ -> Bool_t
  | (Lt|Let|Gt|Get), Unclear_t _, Int_t -> Bool_t
  | (Lt|Let|Gt|Get), Int_t, Unclear_t _ -> Bool_t
  | (Lt|Let|Gt|Get), Unclear_t _, Float_t -> Bool_t
  | (Lt|Let|Gt|Get), Float_t, Unclear_t _ -> Bool_t

  | Head, a, List_t t when a = t -> List_t t
  | Head, a, List_t _ when is_unclear_list tb -> List_t a

  | a, b, c -> failwith (Printf.sprintf "(Typing) binary operation (%s) used with arguments of incorrect types: (%s) and (%s)" (binop_str a) (type_obj_str b) (type_obj_str c))

let type_asc_binop = fun op tr ->
  if !debug_typing then
    Printf.printf "%stype_asc_binop op:%s tr:%s\n" (String.make !debug_print_indent '\t') (binop_str op) (type_obj_str tr);
  match op, tr with
  | Add, Int_t -> (Int_t, Int_t)
  | Add, Float_t -> (Float_t, Float_t)
  | Add, Bool_t -> (Bool_t, Bool_t)
  | Add, String_t -> (String_t, String_t)
  | Add, List_t t -> (List_t t, List_t t)
  | Add, Unclear_t i -> (Unclear_t i, Unclear_t i)

  | Sub, Int_t -> (Int_t, Int_t)
  | Sub, Float_t -> (Float_t, Float_t)
  | Sub, List_t t -> (List_t t, List_t t)
  | Sub, Unclear_t i -> (Unclear_t i, Unclear_t i)

  | Mult, Int_t -> (Int_t, Int_t)
  | Mult, Float_t -> (Float_t, Float_t)
  | Mult, Bool_t -> (Bool_t, Bool_t)
  | Mult, String_t -> (String_t, Int_t)
  | Mult, List_t t -> (List_t t, Int_t)
  | Mult, Unclear_t i -> (Unclear_t i, Unclear_t (next_unclear_id()))

  | Div, Int_t -> (Int_t, Int_t)
  | Div, Float_t -> (Float_t, Float_t)
  | Div, Unclear_t i -> (Unclear_t i, Unclear_t i)

  | Mod, Int_t -> (Int_t, Int_t)
  | Mod, Float_t -> (Float_t, Float_t)
  | Mod, Unclear_t i -> (Unclear_t i, Unclear_t i)

  | Same, Bool_t -> (Unclear_t (next_unclear_id()), Unclear_t (next_unclear_id()))

  | Different, Bool_t -> (Unclear_t (next_unclear_id()), Unclear_t (next_unclear_id()))

  | (Lt|Let|Gt|Get), Bool_t -> (Unclear_t (next_unclear_id()), Unclear_t (next_unclear_id()))

  | Head, List_t t -> (t, List_t t)
  | Head, t when is_unclear_list t -> (t, List_t t)

  | a, b -> failwith (Printf.sprintf "(Typing) binary operation (%s) was inferred incorrect output of type (%s)" (binop_str a) (type_obj_str b))

let index_of e l = 
  if !debug_typing then
    Printf.printf "%sindex_of\n" (String.make !debug_print_indent '\t');
  let rec index_rec i = function
    | [] -> failwith "index_of: not found"
    | hd::tl -> if hd = e then i else index_rec (i+1) tl
  in
  index_rec 0 l

let type_desc_fcall = fun ft atls ->
  if !debug_typing then
    Printf.printf "%stype_desc_fcall ft:%s atls:%s\n" (String.make !debug_print_indent '\t') (type_obj_str ft) (String.concat "; " (List.map type_obj_str atls));
  match ft with
  | Function_t(tpins, tpout) ->
      let rec local = fun tpinsl atlsl ->
        (*Printf.printf "type_desc_fcall local:\n[%s] vs [%s]\n" (String.concat ", " (List.map type_obj_str tpinsl)) (String.concat ", " (List.map type_obj_str atlsl));*)
        begin match tpinsl, atlsl with
          | [], [] -> begin match tpout with
                            | Unclear_t _ -> let pos = index_of tpout tpins in List.nth atls pos
                            | a -> a end
          | a, [] -> Function_t(a, tpout)
          | [], _::_ -> failwith "(Typing) too many arguments in function call"
          | a::tla, b::tlb when a = b -> local tla tlb
          | Unclear_t _::tla, _::tlb -> local tla tlb
          | Any_t::tla, _::tlb -> local tla tlb
          | _::tla, Unclear_t _::tlb -> local tla tlb
          | a::_, b::_ -> failwith (Printf.sprintf "(Typing) argument of incorrect type: required (%s) but got (%s)" (type_obj_str a) (type_obj_str b)) end in
      local tpins atls
  | Unclear_t _ -> Unclear_t (next_unclear_id())
  | _ -> failwith (Printf.sprintf "(Typing) tried to call a non function (type of object is %s)" (type_obj_str ft))

let rec type_desc_stat = fun tcontext st ->
  if !debug_typing then
    Printf.printf "%stype_desc_stat st:%s\n" (String.make !debug_print_indent '\t') (stat_str st);
    incr debug_print_indent;
  match st with
  | STAT_NOOP -> decr debug_print_indent; None_t
  | STAT_EXPR e -> 
    let res = type_desc_expr tcontext e in
    let _ = type_asc_expr tcontext e res in decr debug_print_indent;res
  | STAT_ASSIGN (el, elv, stn) ->
    type_assign tcontext el (List.map (fun x -> type_desc_expr tcontext x) elv);
    let res = type_desc_stat tcontext stn in
    decr debug_print_indent;
    res
  | STAT_ASSIGN_TOPLEVEL (el, elv) ->
    type_assign tcontext el (List.map (fun x -> type_desc_expr tcontext x) elv);
    decr debug_print_indent;
    None_t
  | STAT_IF (e, sti, ste) -> 
    let _ = type_desc_expr tcontext e in
    begin match type_desc_stat (copy_tcontext tcontext) sti, type_desc_stat (copy_tcontext tcontext) ste with
          | a, b when a = b -> decr debug_print_indent; a
          | a, b -> failwith (Printf.sprintf "(Typing) blocks in IF/ELSE statement should have same return types, but got (%s) and (%s)" (type_obj_str a) (type_obj_str b)) end
  | STAT_DROPVALUE (e, stn) -> 
    let _ = type_desc_expr tcontext e in
    let res = type_desc_stat tcontext stn in
    decr debug_print_indent; res

and type_asc_stat = fun tcontext st t ->
  if !debug_typing then
    Printf.printf "%s type_asc_stat st:%s t:%s\n" (String.make !debug_print_indent '\t') (stat_str st) (type_obj_str t);
    incr debug_print_indent;
  let _ = match st with
  | STAT_NOOP -> None_t
  | STAT_EXPR e -> type_asc_expr tcontext e t
  | STAT_ASSIGN (_, _, _(*el, elv, _*)) -> None_t (*type_asc_assign tcontext el elv*)
  | STAT_ASSIGN_TOPLEVEL (_, _(*el, elv*)) -> None_t (*type_asc_assign tcontext el elv*)
  | STAT_IF (e, sti, ste) -> 
    type_asc_stat (copy_tcontext tcontext) sti t;
    type_asc_stat (copy_tcontext tcontext) ste t;
    type_asc_expr tcontext e t
  | STAT_DROPVALUE (e, _) -> type_asc_expr tcontext e t in decr debug_print_indent

and type_asc_expr = fun tcontext e t ->
  if !debug_typing then
    Printf.printf "%s type_asc_expr e:%s t:%s\n" (String.make !debug_print_indent '\t') (expr_str e) (type_obj_str t);
  incr debug_print_indent;
  let res = match e with
  | EXPR_LITERAL l ->
    let te = type_desc_literal tcontext l in
    begin match te with
    | a when a = t -> a
    | Unclear_t n ->
      Hashtbl.iter (fun i vl -> if vl = Unclear_t n then Hashtbl.add tcontext.type_variables i t else ()) tcontext.type_variables;
      t
    | a when is_unclear_list t && is_unclear_list a -> t
    | a when is_unclear_list a || is_unclear_list t -> a
    | a when t = Any_t -> a
    | a -> failwith (Printf.sprintf "(Typing) literal was inferred type (%s) but has type (%s)" (type_obj_str t) (type_obj_str a)) end
  | EXPR_UNARY (uo, e) -> let te = type_asc_unop uo t in
    if te = t then let _ = type_asc_expr tcontext e te in te else failwith (Printf.sprintf "(Typing) unary operation output (%s) was inferred type (%s) but has type (%s)" (unop_str uo) (type_obj_str t) (type_obj_str te))
  | EXPR_BINARY (bo, e1, e2) -> let (t1, t2) = type_asc_binop bo t in
    let _ = type_asc_expr tcontext e1 t1 in
    let _ = type_asc_expr tcontext e2 t2 in
    t
  | EXPR_FCALL (ef, eal) ->
    let tf = type_desc_expr tcontext ef in
    begin match tf with
    | Function_t (tpins, tpout) ->
      if tpout = t || is_unclear_list tpout then
        let _ = List.map2 (fun x y -> type_asc_expr tcontext x y) eal tpins in
        tpout
      else failwith (Printf.sprintf "(Typing) function was inferred output type (%s) but got (%s)" (type_obj_str t) (type_obj_str tpout))
    | Unclear_t n -> let tfo = type_asc_expr tcontext ef (Function_t(List.map (fun x -> type_asc_expr tcontext x (type_desc_expr tcontext x)) eal, t)) in
      Hashtbl.iter (fun i vl -> if vl = Unclear_t n then Hashtbl.add tcontext.type_variables i tfo else ()) tcontext.type_variables;
      t
    | a -> failwith (Printf.sprintf "(Typing) function call was expecting something callable, but got (%s)" (type_obj_str a)) end
  | EXPR_IDENTIFIER i ->
    begin match Hashtbl.find_opt tcontext.type_variables i with
    | Some ti when t = ti -> ti
    | Some (Unclear_t n) ->
      Hashtbl.iter (fun id vl ->
        if vl = (Unclear_t (n)) then
          Hashtbl.add tcontext.type_variables id t
        else ()) tcontext.type_variables; t
    | Some a -> failwith (Printf.sprintf "(Typing) variable was expected to have type (%s) but got (%s)" (type_obj_str t) (type_obj_str a))
    | None -> failwith (Printf.sprintf "(Typing) variable was expected to have type (%s) but was not in context" (type_obj_str t)) end in
    decr debug_print_indent; res

and type_desc_expr = fun tcontext e ->
  if !debug_typing then
    Printf.printf "%s type_desc_expr e:%s\n" (String.make !debug_print_indent '\t') (expr_str e);
  incr debug_print_indent;
  let res = match e with
  | EXPR_LITERAL l -> type_desc_literal tcontext l
  | EXPR_UNARY (uo, e) -> 
      let t = type_desc_expr tcontext e in
      type_desc_unop uo t
  | EXPR_BINARY (bo, ea, eb) ->
      let ta, tb = (type_desc_expr tcontext ea), (type_desc_expr tcontext eb) in
      type_desc_binop bo ta tb
  | EXPR_FCALL (ef, eal) -> 
      let tf = (type_desc_expr tcontext ef) in
      let tls = List.map (fun e -> type_desc_expr tcontext e) eal in
      type_desc_fcall tf tls
  | EXPR_IDENTIFIER id -> begin match Hashtbl.find_opt tcontext.type_variables id with
                            | Some t -> t
                            | None -> failwith (Printf.sprintf "(Typing) Identifier \"%s\" not found" id) end in
  decr debug_print_indent; res

and type_of_list = fun tcontext ls tacc ->
  if !debug_typing then
    Printf.printf "%stype_of_list ls:%s tacc:%s\n" (String.make !debug_print_indent '\t') (String.concat "; " (List.map expr_str ls)) (type_obj_str tacc);
  let res = begin match ls with
  | [] -> List_t (tacc)
  | hd::tl -> begin match type_desc_expr tcontext hd, tacc with
                    | t, b when t = b -> type_of_list tcontext tl t
                    | t, Unclear_t _ -> type_of_list tcontext tl t
                    | t, b when is_unclear_list t -> type_of_list tcontext tl b
                    | a, b -> failwith (Printf.sprintf "(Typing) element of type (%s) found in list of type (%s)" (type_obj_str a) (type_obj_str b)) end end in
  decr debug_print_indent; res

and type_desc_literal = fun tcontext l ->
  if !debug_typing then
    Printf.printf "%stype_desc_literal l:%s\n" (String.make !debug_print_indent '\t') (lit_str l);
  incr debug_print_indent;
  let res = match l with
  | LITERAL_INT _ -> Int_t
  | LITERAL_FLOAT _ -> Float_t
  | LITERAL_BOOL _ -> Bool_t
  | LITERAL_STRING _ -> String_t
  | LITERAL_LIST l ->
      type_of_list tcontext l (Unclear_t (next_unclear_id()))
  | LITERAL_NONE -> None_t
  | LITERAL_FUNCTION(argnames, stf) -> type_function tcontext argnames stf in
  decr debug_print_indent; res

and type_function = fun tcontext argnames stf ->
  if !debug_typing then
    Printf.printf "%stype_function argnames:%s stf:%s\n" (String.make !debug_print_indent '\t') (String.concat "; " argnames) (stat_str stf);
  incr debug_print_indent;
  let loc_ctx = copy_tcontext tcontext in
  let arg_temp_types = List.map (fun _ -> (Unclear_t (next_unclear_id()))) argnames in
  List.iter2 (fun x nm-> Hashtbl.add (loc_ctx.type_variables) nm x) arg_temp_types argnames;
  let tpout = type_desc_stat loc_ctx stf in
  decr debug_print_indent;
  Function_t(List.map (fun x -> Hashtbl.find (loc_ctx.type_variables) x) (List.rev argnames), tpout)

and type_assign = fun tcontext el elv ->
  if !debug_typing then
    Printf.printf "%stype_assign el:%s elv:%s\n" (String.make !debug_print_indent '\t') (String.concat "; " (List.map expr_str el)) (String.concat "; "(List.map type_obj_str elv));
  incr debug_print_indent;
  let rec assign_values = fun vars values ->
    match vars, values with
    | [], [] -> ()
    | [], _ -> ()
    | (EXPR_IDENTIFIER i)::tlv, [] -> Hashtbl.add tcontext.type_variables i None_t;
                                      assign_values tlv []
    | (EXPR_LITERAL(LITERAL_LIST l))::tlv, [] ->
      assign_values l [];
      assign_values tlv []
    | (EXPR_IDENTIFIER i)::tlv, hde::tle -> Hashtbl.add tcontext.type_variables i hde;
                                            assign_values tlv tle
    | (EXPR_BINARY(Head, EXPR_IDENTIFIER hdi, tli))::tlv, List_t(a)::tle ->
        Hashtbl.add tcontext.type_variables hdi a;
        type_assign tcontext [tli] [List_t(a)];
        assign_values tlv tle
    | (EXPR_LITERAL(LITERAL_NONE))::tlv, _::tle -> assign_values tlv tle
    | (EXPR_LITERAL(LITERAL_LIST l))::tlv, (List_t(a))::tle -> type_assign tcontext l (List.map (fun _ -> a) l); assign_values tlv tle
    | a::_, b::_ -> failwith (Printf.sprintf "(Typing) couldn't assign value of type (%s) to expression (%s)" (type_obj_str b) (expr_str a))
    | _, _ -> failwith "(Typing) error while checking assignment types" in
  let res = assign_values el elv in
  decr debug_print_indent; res