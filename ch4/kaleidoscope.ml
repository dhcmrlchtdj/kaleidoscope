let main () =
    let char_stream = Stream.of_channel stdin in
    let token_stream = Lexer.lex char_stream in

    (* optimizer *)
    Llvm_all_backends.initialize();
    let triple = Llvm_target.Target.default_triple () in
    let target =
        match Llvm_target.Target.by_name "x86-64" with
            | Some t -> t
            | None -> failwith "target not found"
    in
    let machine = Llvm_target.TargetMachine.create ~triple:triple target in
    let the_fpm = Llvm.PassManager.create_function Codegen.the_module in
    Llvm_target.TargetMachine.add_analysis_passes the_fpm machine;
    Llvm_scalar_opts.add_instruction_combination the_fpm;
    Llvm_scalar_opts.add_reassociation the_fpm;
    Llvm_scalar_opts.add_gvn the_fpm;
    Llvm_scalar_opts.add_cfg_simplification the_fpm;
    ignore (Llvm.PassManager.initialize the_fpm);

    Toplevel.main_loop the_fpm token_stream;
    Llvm.dump_module Codegen.the_module

let () = main ()

(* 4+5; *)
(* extern sin(x); *)
(* sin(1.0); *)
(* extern cos(x); *)
(* def foo(x) sin(x)*sin(x) + cos(x)*cos(x); *)
(* foo(4.0); *)
