(* FStyping.ml *)

open FStypes;;
open FSobjs;;
open FSsyntax;;

(* TODO: ascendant type-checking buggy (a, b, c -> a(b(c))) not giving expected type, lists not verified for same types in all elements, recursive functions not working *)

let rec first_elements_of_list = fun ls n ->
  match ls with
  | [] -> []
  | hd::tl -> if n <= 1 then [hd]
              else hd::(first_elements_of_list tl (n - 1));;

let last_elements_of_list = fun ls n ->
  first_elements_of_list (List.rev ls) (List.length ls - n);;


type type_context = {
  mutable type_variables: (string, type_obj) Hashtbl.t
}

let copy_tcontext = fun tctx ->
  {type_variables = Hashtbl.copy (tctx.type_variables)}

let rec get_type_of_obj = fun obj ->
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
  let tctx = {type_variables = Hashtbl.create (Hashtbl.length ctx.variables)} in
  Hashtbl.iter (fun id vl -> Hashtbl.add (tctx.type_variables) id (get_type_of_obj vl)) ctx.variables;
  tctx

let type_check_args = fun tot_args tpins ->
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

let type_desc_unop = fun op t ->
  match op,t with
  | Not, Bool_t -> Bool_t
  | Not, Unclear_t _ -> Bool_t
  | Uminus, Int_t -> Int_t
  | Uminus, Float_t -> Float_t
  | Uminus, Unclear_t i -> Unclear_t i
  | _, _ -> failwith "Error in unary operation (desc)"

let type_asc_unop = fun op tr ->
  match op, tr with
  | Not, Bool_t -> Bool_t
  | Not, Unclear_t _ -> Bool_t
  | Uminus, Int_t -> Int_t
  | Uminus, Float_t -> Float_t
  | Uminus, Unclear_t i -> Unclear_t i
  | _, _ -> failwith "Error in unary operation (asc)"

let type_desc_binop = fun op ta tb ->
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

  | _, _, _ -> failwith "Error in binary operation (desc)"

let type_asc_binop = fun op tr ->
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

  | _, _ -> failwith "Error in binary operation (asc)"

let index_of e l = 
  let rec index_rec i = function
    | [] -> failwith "notfound"
    | hd::tl -> if hd = e then i else index_rec (i+1) tl
  in
  index_rec 0 l

let type_desc_fcall = fun ft atls ->
  match ft with
  | Function_t(tpins, tpout) ->
      let rec local = fun tpinsl atlsl ->
        (*Printf.printf "type_desc_fcall:\n[%s] vs [%s]\n" (String.concat ", " (List.map type_obj_str tpinsl)) (String.concat ", " (List.map type_obj_str atlsl));*)
        begin match tpinsl, atlsl with
          | [], [] -> begin match tpout with
                            | Unclear_t _ -> let pos = index_of tpout tpins in List.nth atls pos
                            | a -> a end
          | _, [] -> Function_t(tpinsl, tpout)
          | a::tla, b::tlb when a = b -> local tla tlb
          | Unclear_t _::tla, _::tlb -> local tla tlb
          | Any_t::tla, _::tlb -> local tla tlb
          | _::tla, Unclear_t _::tlb -> local tla tlb
          | _, _ -> failwith "Error in function call (desc)" end in
      local tpins atls
  | Unclear_t _ -> Unclear_t (next_unclear_id())
  | _ -> failwith "Trying to call a non-function"

let rec type_desc_stat = fun tcontext st ->
  match st with
  | STAT_NOOP -> None_t
  | STAT_EXPR e -> 
    let res = type_desc_expr tcontext e in
    let _ = type_asc_expr tcontext e res in res
  | STAT_ASSIGN (el, elv, stn) ->
    type_assign tcontext el (List.map (fun x -> type_desc_expr tcontext x) elv);
    type_desc_stat tcontext stn
  | STAT_ASSIGN_TOPLEVEL (el, elv) ->
    type_assign tcontext el (List.map (fun x -> type_desc_expr tcontext x) elv);
    None_t
  | STAT_IF (e, sti, ste) -> 
    let _ = type_desc_expr tcontext e in
    begin match type_desc_stat (copy_tcontext tcontext) sti, type_desc_stat (copy_tcontext tcontext) ste with
          | a, b when a = b -> a
          | _ -> failwith "Error: two if blocks do not have the same return type" end
  | STAT_DROPVALUE (e, stn) -> 
    let _ = type_desc_expr tcontext e in
    type_desc_stat tcontext stn

and type_asc_stat = fun tcontext st t ->
  let _ = match st with
  | STAT_NOOP -> None_t
  | STAT_EXPR e -> type_asc_expr tcontext e t
  | STAT_ASSIGN (_, _, _(*el, elv, _*)) -> None_t (*type_asc_assign tcontext el elv*)
  | STAT_ASSIGN_TOPLEVEL (_, _(*el, elv*)) -> None_t (*type_asc_assign tcontext el elv*)
  | STAT_IF (e, sti, ste) -> 
    type_asc_stat (copy_tcontext tcontext) sti t;
    type_asc_stat (copy_tcontext tcontext) ste t;
    type_asc_expr tcontext e t
  | STAT_DROPVALUE (e, _) -> type_asc_expr tcontext e t in ()

and type_asc_expr = fun tcontext e t ->
  match e with
  | EXPR_LITERAL l ->
    let te = type_desc_literal tcontext l in
    begin match te with
    | a when a = t -> a
    | Unclear_t n ->
      Hashtbl.iter (fun i vl -> if vl = Unclear_t n then Hashtbl.add tcontext.type_variables i t else ()) tcontext.type_variables;
      t
    | _ -> failwith "Error: literal has wrong type" end
  | EXPR_UNARY (uo, e) -> let te = type_asc_unop uo t in
    if te = t then let _ = type_asc_expr tcontext e te in te else failwith "Error: unary operation has wrong type"
  | EXPR_BINARY (bo, e1, e2) -> let (t1, t2) = type_asc_binop bo t in
    let _ = type_asc_expr tcontext e1 t1 in
    let _ = type_asc_expr tcontext e2 t2 in
    t
  | EXPR_FCALL (ef, eal) ->
    let tf = type_desc_expr tcontext ef in
    begin match tf with
    | Function_t (tpins, tpout) ->
      if tpout = t then
        let _ = List.map2 (fun x y -> type_asc_expr tcontext x y) eal tpins in
        tf
      else failwith "Error: function call has wrong type"
    | Unclear_t n -> let tfo = type_asc_expr tcontext ef (Function_t(List.map (fun x -> type_asc_expr tcontext x (type_desc_expr tcontext x)) eal, t)) in
      Hashtbl.iter (fun i vl -> if vl = Unclear_t n then Hashtbl.add tcontext.type_variables i tfo else ()) tcontext.type_variables;
      tfo
    | _ -> failwith "Error: function call has wrong type" end
  | EXPR_IDENTIFIER i ->
    begin match Hashtbl.find_opt tcontext.type_variables i with
    | Some ti when t = ti -> ti
    | Some (Unclear_t n) ->
      Hashtbl.iter (fun id vl ->
        if vl = (Unclear_t (n)) then
          Hashtbl.add tcontext.type_variables id t
        else ()) tcontext.type_variables; t
    | _ -> failwith "Error: identifier has wrong type" end

and type_desc_expr = fun tcontext e ->
  match e with
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
                            | None -> failwith "Error: Identifier not found" end

and type_of_list = fun tcontext ls tacc ->
  begin match ls with
  | [] -> List_t (tacc)
  | hd::tl -> begin match type_desc_expr tcontext hd, tacc with
                    | t, b when t = b -> type_of_list tcontext tl t
                    | t, Unclear_t _ -> type_of_list tcontext tl t
                    | _ -> failwith "Type error in list literal" end end 

and type_desc_literal = fun tcontext l ->
  match l with
  | LITERAL_INT _ -> Int_t
  | LITERAL_FLOAT _ -> Float_t
  | LITERAL_BOOL _ -> Bool_t
  | LITERAL_STRING _ -> String_t
  | LITERAL_LIST l ->
      type_of_list tcontext l (Unclear_t (next_unclear_id()))
  | LITERAL_NONE -> None_t
  | LITERAL_FUNCTION(argnames, stf) -> type_function tcontext argnames stf

and type_function = fun tcontext argnames stf ->
  let loc_ctx = copy_tcontext tcontext in
  let arg_temp_types = List.map (fun _ -> (Unclear_t (next_unclear_id()))) argnames in
  List.iter2 (fun x nm-> Hashtbl.add (loc_ctx.type_variables) nm x) arg_temp_types argnames;
  let tpout = type_desc_stat loc_ctx stf in
  Function_t(List.map (fun x -> Hashtbl.find (loc_ctx.type_variables) x) (List.rev argnames), tpout)

and type_assign = fun tcontext el elv ->
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
    | _, _ -> failwith "Error in assignment type-checking" in
  assign_values el elv