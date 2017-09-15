let rec main_loop token_stream =
    print_string "ready> ";
    flush stdout;
    match Stream.peek token_stream with
        | None -> ()
        | Some token -> (
                match token with
                    | Token.Whitespace _ ->
                        Stream.junk token_stream;
                    | Token.Def ->
                        ignore (Parser.parse_definition token_stream);
                        print_endline "parsed a function definition."
                    | Token.Extern ->
                        ignore (Parser.parse_extern token_stream);
                        print_endline "parsed an extern."
                    | _ ->
                        ignore (Parser.parse_toplevel token_stream);
                        print_endline "parsed a top-level expr"
            );
            main_loop token_stream
