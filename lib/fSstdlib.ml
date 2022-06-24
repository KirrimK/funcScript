(* FSstdlib.ml *)

open FSeval;;

let std_context = {
    variables = Hashtbl.create 50;
    types = Hashtbl.create 50;
  }

let fs_print = Eval_OCaml_Function([], 
  (fun _ objls -> 
    Printf.printf "%s%!" (eval_obj_str (List.hd objls));
    Eval_None),
  [Any_t], None_t);;
let fs_println = Eval_OCaml_Function([], 
  (fun _ objls -> 
    Printf.printf "%s\n%!" (eval_obj_str (List.hd objls));
    Eval_None),
  [Any_t], None_t);;

Hashtbl.add std_context.variables "print" fs_print;;
Hashtbl.add std_context.variables "println" fs_println;;

let fs_float = Eval_OCaml_Function([],
  (fun _ objls ->
    match List.hd objls with
    | Eval_Int i -> Eval_Float(float_of_int i)
    | _ -> failwith "error while converting to float"),
  [Int_t], Float_t);;

let fs_int = Eval_OCaml_Function([],
  (fun _ objls ->
    match List.hd objls with
    | Eval_Float f -> Eval_Int(int_of_float f)
    | _ -> failwith "error while truncating to int"),
  [Float_t], Int_t);;

Hashtbl.add std_context.variables "float" fs_float;;
Hashtbl.add std_context.variables "int" fs_int;;