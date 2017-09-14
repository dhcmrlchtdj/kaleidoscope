(* top ::= definition | external | expression | ';' *)
let rec main_loop token_stream =
    match Stream.peek token_stream with
        | None -> ()

        | Some (Token.Kwd ';') ->
            Stream.junk token_stream;
            main_loop token_stream

        | Some token -> (
                match token with
                    | Token.Def ->
                        Stream.junk token_stream;
                        print_endline "parsed a function definition.";
                    | Token.Extern ->
                        Stream.junk token_stream;
                        print_endline "parsed an extern.";
                    | _ ->
                        Stream.junk token_stream;
                        print_endline "parsed a top-level expr";
            );
            print_string "ready> ";
            flush stdout;
            main_loop token_stream
