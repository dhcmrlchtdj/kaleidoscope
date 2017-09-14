let rec lex chars =
    let rec lex_ident buffer =
        match Stream.peek chars with
            | None -> failwith "lex_indent"
            | Some x -> (
                    match x with
                        | 'A'..'Z' | 'a'..'z' | '0'..'9' as c ->
                            Stream.junk chars;
                            Buffer.add_char buffer c;
                            lex_ident buffer
                        | _ -> (
                                let s = Buffer.contents buffer in
                                match s with
                                    | "def" -> Token.Def
                                    | "extern" -> Token.Extern
                                    | id -> Token.Ident id
                            )
                )
    in
    let rec lex_number buffer =
        match Stream.peek chars with
            | None -> failwith "lex_number"
            | Some x -> (
                    match x with
                        | '0'..'9' | '.' as c ->
                            Stream.junk chars;
                            Buffer.add_char buffer c;
                            lex_number buffer
                        | _ -> (
                                let s = Buffer.contents buffer in
                                Token.Number (float_of_string s)
                            )
                )
    in
    let rec lex_comment buffer =
        match Stream.peek chars with
            | None -> failwith "lex_comment"
            | Some x -> (
                    match x with
                        | '\n' ->
                            Stream.junk chars;
                            let s = Buffer.contents buffer in
                            Token.Comment s
                        | _ as c ->
                            Stream.junk chars;
                            Buffer.add_char buffer c;
                            lex_comment buffer
                )
    in
    let rec lex_stream () =
        match Stream.peek chars with
            | None -> None
            | Some x -> (
                    match x with
                        | ' ' | '\n' | '\r' | '\t' ->
                            Stream.junk chars;
                            lex_stream ()
                        | 'A'..'Z' | 'a'..'z' as c ->
                            Stream.junk chars;
                            let buffer = Buffer.create 1 in
                            Buffer.add_char buffer c;
                            Some (lex_ident buffer)
                        | '0'..'9' as c ->
                            Stream.junk chars;
                            let buffer = Buffer.create 1 in
                            Buffer.add_char buffer c;
                            Some (lex_number buffer)
                        | '#' as c ->
                            Stream.junk chars;
                            let buffer = Buffer.create 1 in
                            Buffer.add_char buffer c;
                            Some (lex_comment buffer)
                        | c ->
                            Some (Token.Kwd c)
                )
    in
    Stream.from (fun i -> lex_stream ())
