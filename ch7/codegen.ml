let context = Llvm.global_context ()
let the_module = Llvm.create_module context "Kaleidoscope"
let builder = Llvm.builder context
let named_values: (string,Llvm.llvalue) Hashtbl.t = Hashtbl.create 10
let double_type = Llvm.double_type context

let create_entry_block_alloca the_function var_name =
    let block = Llvm.entry_block the_function in
    let pos = Llvm.instr_begin block in
    let builder = Llvm.builder_at context pos in
    Llvm.build_alloca double_type var_name builder

let rec codegen_expr = function
    | Ast.Number n ->
        Llvm.const_float double_type n
    | Ast.Variable name ->
        let v =
            try
                Hashtbl.find named_values name
            with
                | Not_found -> failwith "unknown variable name"
        in
        Llvm.build_load v name builder
    | Ast.Unary (op, arg) ->
        let operand = codegen_expr arg in
        let func = "unary" ^ (String.make 1 op) in
        let callee =
            match Llvm.lookup_function func the_module with
                | Some f -> f
                | None -> failwith "invalid unary operator"
        in
        Llvm.build_call callee [|operand|] "unop" builder
    | Ast.Binary (op, lhs, rhs) ->
        (match op with
            | '=' ->
                let name =
                    match lhs with
                        | Ast.Variable n -> n
                        | _ -> failwith "destination of '=' must be a variable"
                in
                let val_ = codegen_expr rhs in
                let variable =
                    try Hashtbl.find named_values name
                    with Not_found -> failwith "unknown Variable name"
                in
                ignore (Llvm.build_store val_ variable builder);
                val_
            | _ ->
                let lhs_val = codegen_expr lhs in
                let rhs_val = codegen_expr rhs in
                (match op with
                    | '+' -> Llvm.build_fadd lhs_val rhs_val "addtmp" builder
                    | '-' -> Llvm.build_fsub lhs_val rhs_val "subtmp" builder
                    | '*' -> Llvm.build_fmul lhs_val rhs_val "multmp" builder
                    | '<' ->
                        let i = Llvm.build_fcmp Llvm.Fcmp.Ult lhs_val rhs_val "cmptmp" builder in
                        Llvm.build_uitofp i double_type "booltmp" builder
                    | _ ->
                        let func = "binary" ^ (String.make 1 op) in
                        let callee =
                            match Llvm.lookup_function func the_module with
                                | Some f -> f
                                | None -> failwith "invalid binary operator"
                        in
                        Llvm.build_call callee [|lhs_val; rhs_val|] "binop" builder
                )
        )
    | Ast.Call (func, args) ->
        let callee =
            match Llvm.lookup_function func the_module with
                | Some f -> f
                | None -> failwith "unknown function referenced"
        in
        let params = Llvm.params callee in
        if Array.length params != Array.length args
        then failwith "incorrect # arguments passed";
        let args = Array.map codegen_expr args in
        Llvm.build_call callee args "calltmp" builder
    | Ast.If (cond_, then_, else_) ->
        let cond = codegen_expr cond_ in
        let zero = Llvm.const_float double_type 0.0 in
        let cond_val = Llvm.build_fcmp Llvm.Fcmp.One cond zero "ifcond" builder in

        let start_bb = Llvm.insertion_block builder in
        let the_function = Llvm.block_parent start_bb in

        let then_bb = Llvm.append_block context "then" the_function in
        Llvm.position_at_end then_bb builder;
        let then_val = codegen_expr then_ in
        let new_then_bb = Llvm.insertion_block builder in

        let else_bb = Llvm.append_block context "else" the_function in
        Llvm.position_at_end else_bb builder;
        let else_val = codegen_expr else_ in
        let new_else_bb = Llvm.insertion_block builder in

        let merge_bb = Llvm.append_block context "ifcont" the_function in
        Llvm.position_at_end merge_bb builder;
        let incoming = [(then_val, new_then_bb); (else_val, new_else_bb)] in
        let phi = Llvm.build_phi incoming "iftmp" builder in

        Llvm.position_at_end start_bb builder;
        ignore (Llvm.build_cond_br cond_val then_bb else_bb builder);
        Llvm.position_at_end new_then_bb builder;
        ignore (Llvm.build_br merge_bb builder);
        Llvm.position_at_end new_else_bb builder;
        ignore (Llvm.build_br merge_bb builder);
        Llvm.position_at_end merge_bb builder;
        phi
    | Ast.For (var_name, start, stop, step, body) ->
        let the_function = Llvm.block_parent (Llvm.insertion_block builder) in
        let alloca = create_entry_block_alloca the_function var_name in
        let start_val = codegen_expr start in
        ignore (Llvm.build_store start_val alloca builder);
        let loop_bb = Llvm.append_block context "loop" the_function in
        ignore (Llvm.build_br loop_bb builder);
        Llvm.position_at_end loop_bb builder;
        let old_val =
            try Some (Hashtbl.find named_values var_name)
            with Not_found -> None
        in
        Hashtbl.add named_values var_name alloca;
        ignore (codegen_expr body);
        let step_val =
            match step with
                | Some step -> codegen_expr step
                | None -> Llvm.const_float double_type 1.0
        in
        let stop_cond_ = codegen_expr stop in
        let cur_var = Llvm.build_load alloca var_name builder in
        let next_var = Llvm.build_fadd cur_var step_val "nextvar" builder in
        ignore (Llvm.build_store next_var alloca builder);
        let zero = Llvm.const_float double_type 0.0 in
        let stop_cond = Llvm.build_fcmp Llvm.Fcmp.One stop_cond_ zero "loopcond" builder in
        let after_bb = Llvm.append_block context "afterloop" the_function in
        ignore (Llvm.build_cond_br stop_cond loop_bb after_bb builder);
        Llvm.position_at_end after_bb builder;
        (match old_val with
            | Some old -> Hashtbl.add named_values var_name old
            | None -> ());
        Llvm.const_null double_type
    | Ast.Var (var_names, body) ->
        let old_bindings = ref [] in
        let the_function = Llvm.block_parent (Llvm.insertion_block builder) in
        Array.iter (fun (var_name, init) ->
            let init_val =
                match init with
                    | Some init -> codegen_expr init
                    | None -> Llvm.const_float double_type 0.0
            in
            let alloca = create_entry_block_alloca the_function var_name in
            ignore (Llvm.build_store init_val alloca builder);
            (try
                let old_value = Hashtbl.find named_values var_name in
                old_bindings := (var_name, old_value) :: !old_bindings;
            with Not_found -> ());
            Hashtbl.add named_values var_name alloca
        ) var_names;
        let body_val = codegen_expr body in
        List.iter (fun (var_name, old_value) ->
            Hashtbl.add named_values var_name old_value
        ) !old_bindings;
        body_val

let rec codegen_proto = function
    | Ast.Prototype (name, args) ->
        let doubles = Array.make (Array.length args) double_type in
        let ft = Llvm.function_type double_type doubles in
        let f =
            match Llvm.lookup_function name the_module with
                | Some f ->
                    if Array.length (Llvm.basic_blocks f) != 0
                    then failwith "redefinition of function";
                    if Array.length (Llvm.params f) != Array.length args
                    then failwith "redefinition of function with different # args";
                    f
                | None ->
                    Llvm.declare_function name ft the_module
        in
        let fiter = fun i a ->
            let n = args.(i) in
            Llvm.set_value_name n a;
            Hashtbl.add named_values n a
        in
        Array.iteri fiter (Llvm.params f);
        f
    | Ast.BinOpPrototype (name, args, prec) ->
        let op = name.[String.length name - 1] in
        Hashtbl.add Parser.binop_precedence op prec;
        codegen_proto (Ast.Prototype (name, args))

let create_argument_allocas the_function proto =
    let args =
        match proto with
            | Ast.Prototype (_, args) | Ast.BinOpPrototype (_, args, _) -> args
    in
    Array.iteri (fun i ai ->
        let var_name = args.(i) in
        let alloca = create_entry_block_alloca the_function var_name in
        ignore (Llvm.build_store ai alloca builder);
        Hashtbl.add named_values var_name alloca;
    ) (Llvm.params the_function)

let codegen_func = function
    | Ast.Function (proto, body) ->
        Hashtbl.clear named_values;
        let the_function = codegen_proto proto in

        let bb = Llvm.append_block context "entry" the_function in
        Llvm.position_at_end bb builder;
        try
            create_argument_allocas the_function proto;
            let ret_val = codegen_expr body in
            ignore (Llvm.build_ret ret_val builder);
            Llvm_analysis.assert_valid_function the_function;
            the_function
        with
            | e ->
                Llvm.delete_function the_function;
                raise e
