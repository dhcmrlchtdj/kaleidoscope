let main () =
    (* Install standard binary operators.
     * 1 is the lowest precedence. *)
    (* Hashtbl.add Parser.binop_precedence '<' 10; *)
    (* Hashtbl.add Parser.binop_precedence '+' 20; *)
    (* Hashtbl.add Parser.binop_precedence '-' 20; *)
    (* Hashtbl.add Parser.binop_precedence '*' 40;    (* highest. *) *)

    print_string "ready> ";
    flush stdout;
    let char_stream = Stream.of_channel stdin in
    let token_stream = Lexer.lex char_stream in
    Toplevel.main_loop token_stream

let _ = main ()
