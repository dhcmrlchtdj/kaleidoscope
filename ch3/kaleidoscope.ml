let main () =
    let char_stream = Stream.of_channel stdin in
    let token_stream = Lexer.lex char_stream in
    Toplevel.main_loop token_stream;
    Llvm.dump_module Codegen.the_module

let () = main ()

(* def foo(x y) x+foo(y, 4.0); *)
(* def foo(x y) x+y y; *)
(* def foo(x y) x+y ); *)
(* extern sin(a); *)
