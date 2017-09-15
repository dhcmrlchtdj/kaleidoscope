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
            | Some token -> (
                    match token with
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
                print_string "ready> ";
                flush stdout;
                loop token_stream
    in
    print_string "ready> ";
    flush stdout;
    loop token_stream
