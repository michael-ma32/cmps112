(* $Id: interp.ml,v 1.6 2019-01-24 19:14:14-08 - - $ *)

open Absyn

exception Unimplemented of string
let unimpl reason = raise (Unimplemented reason)

let want_dump = ref false

let rec eval_expr (expr : Absyn.expr) : float = match expr with
    | Number number -> number
    | Memref memref -> 
        (match memref with
            | Arrayref (ident, expr) -> 
                let dimarray = Hashtbl.find Tables.array_table ident in
                let array_index_float = eval_expr expr in
                let array_index_int = int_of_float array_index_float in
                dimarray.(array_index_int)
            | Variable (ident) -> 
                let f = Hashtbl.find Tables.variable_table ident in
                f
        )
    | Unary (oper, expr) -> 
        let e1 = eval_expr expr in 
        let f = Hashtbl.find Tables.unary_fn_table oper in
        f e1 
    | Binary (oper, expr1, expr2) ->
        let e1 = eval_expr expr1 in
        let e2 = eval_expr expr2 in
        let f = Hashtbl.find Tables.binary_fn_table oper in
        f e1 e2

let eval_pervasive (expr: Absyn.expr) = match expr with 
    | Number number -> unimpl "number error"
    | Memref memref -> unimpl "memref error"
    | Unary (oper, expr) -> unimpl "unary error"
    | Binary (oper, expr1, expr2) ->
        let e1 = eval_expr expr1 in
        let e2 = eval_expr expr2 in
        let f = Hashtbl.find Tables.binary_pervasive_fn_table oper in
        f e1 e2

let interp_if expr label =
    let checkif = eval_pervasive expr in match checkif with 
        | true -> Some (Hashtbl.find Tables.label_table label)
        | false -> None 

let interp_goto label = 
    Some (Hashtbl.find Tables.label_table label)

let interp_dim ident expr = 
    let array_index_float = eval_expr expr in
    let array_index_int = int_of_float array_index_float in
    let array_index_basic = array_index_int + 1 in
    let dimarray = Array.make array_index_basic 0.0 in
    Hashtbl.add Tables.array_table ident dimarray (* "a" "dimarray" *)

let interp_let memref rightexpr = match memref with 
    | Arrayref (ident, leftexpr) ->
        let dimarray = Hashtbl.find Tables.array_table ident in
        let array_index_float = eval_expr leftexpr in
        let array_index_int = int_of_float array_index_float in
        let rightexpr_value = eval_expr rightexpr in
        dimarray.(array_index_int) <- rightexpr_value
    | Variable (ident) -> 
        let e1 = eval_expr rightexpr in
        Hashtbl.add Tables.variable_table ident e1

let interp_print (print_list : Absyn.printable list) =
    let print_item item =
        (print_string " ";
         match item with
         | String string ->
           let regex = Str.regexp "\"\\(.*\\)\""
           in print_string (Str.replace_first regex "\\1" string)
         | Printexpr expr ->
           print_float (eval_expr expr))
    in (List.iter print_item print_list; print_newline ())

let interp_input (memref_list : Absyn.memref list) =
    let input_number memref =
        try  let number = Etc.read_number () in 
            match memref with
                | Arrayref (ident, leftexpr) -> unimpl "array error"
                | Variable (ident) ->
                    Hashtbl.add Tables.variable_table ident number
        with End_of_file -> 
            (match memref with 
                | Arrayref (ident, leftexpr) -> 
                    Hashtbl.add Tables.variable_table ident nan;
                    Hashtbl.add Tables.variable_table "eof" 1.0
                | Variable (ident) ->
                    Hashtbl.add Tables.variable_table ident nan;
                    Hashtbl.add Tables.variable_table "eof" 1.0
            )
    in List.iter input_number memref_list

let interp_stmt (stmt : Absyn.stmt) = match stmt with
    | Dim (ident, expr) -> interp_dim ident expr; None
    | Let (memref, expr) -> interp_let memref expr; None
    | Goto label -> interp_goto label
    | If (expr, label) -> interp_if expr label
    | Print print_list -> interp_print print_list; None
    | Input memref_list -> interp_input memref_list; None

let rec interpret (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::otherlines -> match firstline with
      | _, _, None -> interpret otherlines
      | _, _, Some stmt -> 
        let next_line = interp_stmt stmt 
        in match next_line with
            | None -> interpret otherlines
            | Some line -> interpret line

let interpret_program program =
    (Tables.init_label_table program; 
     if !want_dump then Tables.dump_label_table ();
     if !want_dump then Dumper.dump_program program;
     interpret program)

