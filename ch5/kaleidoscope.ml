let main input =
    let char_stream = input in
    let token_stream = Lexer.lex char_stream in

    (* optimizer *)
    Llvm_all_backends.initialize();
    let triple = Llvm_target.Target.default_triple () in
    let target =
        match Llvm_target.Target.by_name "x86-64" with
            | Some t -> t
            | None -> failwith "target not found"
    in
    let machine = Llvm_target.TargetMachine.create ~triple target in
    let the_fpm = Llvm.PassManager.create_function Codegen.the_module in
    Llvm_target.TargetMachine.add_analysis_passes the_fpm machine;
    Llvm_scalar_opts.add_instruction_combination the_fpm;
    Llvm_scalar_opts.add_reassociation the_fpm;
    Llvm_scalar_opts.add_gvn the_fpm;
    Llvm_scalar_opts.add_cfg_simplification the_fpm;
    ignore (Llvm.PassManager.initialize the_fpm);

    Toplevel.main_loop the_fpm token_stream;
    print_newline ();
    Llvm.dump_module Codegen.the_module


(* let () = main (Stream.of_string "def fib(x) if x < 3 then 1 else fib(x-1)+fib(x-2); fib (5);") *)
let () = main (Stream.of_string "def loop(n) for i = 1.0, i < n, 1.0 in i;")
(* let () = main (Stream.of_channel stdin) *)

(* def fib(x) if x < 3 then 1 else fib(x-1)+fib(x-2); *)
(* fib (5); *)
(* def loop(n) for i = 1, i < n, 1.0 in i; *)
(* loop (5); *)
