let main () =
    let char_stream = Stream.of_channel stdin in
    let token_stream = Lexer.lex char_stream in
    Toplevel.main_loop token_stream;
    Llvm.dump_module Codegen.the_module

let () = main ()

(* 4+5; *)
(* def foo(a b) a*a + 2*a*b + b*b; *)
(* def bar(a) foo(a, 4.0) + bar(31337); *)
(* extern cos(x); *)
(* cos(1.234); *)
