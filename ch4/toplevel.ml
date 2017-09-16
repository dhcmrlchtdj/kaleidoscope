let main_loop token_stream =
    let rec consume_whitespace token_stream =
        match Stream.peek token_stream with
            | Some (Token.Whitespace _) ->
                Stream.junk token_stream;
                consume_whitespace token_stream
            | _ -> None
    and loop token_stream =
        let _ = consume_whitespace token_stream in
        match Stream.peek token_stream with
            | None -> ()
            | Some (Token.Kwd ';') ->
                Stream.junk token_stream;
                loop token_stream
            | Some token -> (
                    match token with
                        | Token.Def ->
                            let e = Parser.parse_definition token_stream in
                            print_endline "parsed a function definition.";
                            Llvm.dump_value (Codegen.codegen_func e)
                        | Token.Extern ->
                            let e = Parser.parse_extern token_stream in
                            print_endline "parsed an extern.";
                            Llvm.dump_value (Codegen.codegen_proto e)
                        | _ ->
                            let e = Parser.parse_toplevel token_stream in
                            print_endline "parsed a top-level expr";
                            Llvm.dump_value (Codegen.codegen_func e)
                );
                print_string "ready> ";
                flush stdout;
                loop token_stream
    in
    print_string "ready> ";
    flush stdout;
    loop token_stream
