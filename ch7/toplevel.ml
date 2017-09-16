let main_loop the_fpm token_stream =
    let rec consume_whitespace token_stream =
        match Stream.peek token_stream with
            | Some (Token.Whitespace _) ->
                Stream.junk token_stream;
                consume_whitespace token_stream
            | _ -> None
    and optimize func =
        Llvm.dump_value func;
        let optimized = Llvm.PassManager.run_function func the_fpm in
        if optimized then (
            print_endline "\nafter optimized";
            Llvm.dump_value func
        )
    and loop token_stream =
        let _ = consume_whitespace token_stream in
        match Stream.peek token_stream with
            | None -> ()
            | Some (Token.Kwd ';') ->
                Stream.junk token_stream;
                loop token_stream
            | Some token ->
                (try
                    match token with
                        | Token.Def ->
                            let e = Parser.parse_definition token_stream in
                            print_endline "parsed a function definition.";
                            let the_function = Codegen.codegen_func e in
                            optimize the_function
                        | Token.Extern ->
                            let e = Parser.parse_extern token_stream in
                            print_endline "parsed an extern.";
                            Llvm.dump_value (Codegen.codegen_proto e)
                        | _ ->
                            let e = Parser.parse_toplevel token_stream in
                            print_endline "parsed a top-level expr";
                            let the_function = Codegen.codegen_func e in
                            optimize the_function
                with
                    | _ ->
                        Stream.junk token_stream;
                        Token.print_token token);
                print_string "\n\n\nready> ";
                flush stdout;
                loop token_stream
    in
    print_string "ready> ";
    flush stdout;
    loop token_stream
