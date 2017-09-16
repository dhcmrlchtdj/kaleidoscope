let lex chars =
    let rec map_chars f =
        match Stream.peek chars with
            | None -> None
            | Some x -> f x
    and create_buf c =
        let buffer = Buffer.create 1 in
        Buffer.add_char buffer c;
        buffer
    and lex_ident buffer =
        map_chars (function
            | 'A'..'Z' | 'a'..'z' | '0'..'9' as c ->
                Stream.junk chars;
                Buffer.add_char buffer c;
                lex_ident buffer
            | _ -> (
                    let s = Buffer.contents buffer in
                    match s with
                        | "def" -> Some Token.Def
                        | "extern" -> Some Token.Extern
                        | "if" -> Some Token.If
                        | "then" -> Some Token.Then
                        | "else" -> Some Token.Else
                        | "for" -> Some Token.For
                        | "in" -> Some Token.In
                        | id -> Some (Token.Ident id)
                )
        )
    and lex_number buffer =
        map_chars (function
            | '0'..'9' | '.' as c ->
                Stream.junk chars;
                Buffer.add_char buffer c;
                lex_number buffer
            | _ -> (
                    let s = Buffer.contents buffer in
                    Some (Token.Number (float_of_string s))
                )
        )
    and lex_comment buffer =
        map_chars (function
            | '\n' ->
                Stream.junk chars;
                let s = Buffer.contents buffer in
                Some (Token.Comment s)
            | _ as c ->
                Stream.junk chars;
                Buffer.add_char buffer c;
                lex_comment buffer
        )
    and lex_whitespace buffer =
        map_chars (function
            | '\n' ->
                Stream.junk chars;
                let s = Buffer.contents buffer in
                Some (Token.Whitespace s)
            | ' ' | '\r' | '\t' as c ->
                Stream.junk chars;
                Buffer.add_char buffer c;
                lex_whitespace buffer
            | _ ->
                let s = Buffer.contents buffer in
                Some (Token.Whitespace s)
        )
    and lex_stream () =
        map_chars (fun c ->
            Stream.junk chars;
            match c with
                | '\n' ->
                    Some (Token.Whitespace (Char.escaped c))
                | ' ' | '\r' | '\t' ->
                    lex_whitespace (create_buf c)
                | 'A'..'Z' | 'a'..'z' ->
                    lex_ident (create_buf c)
                | '0'..'9' ->
                    lex_number (create_buf c)
                | '#' ->
                    lex_comment (create_buf c)
                | _ ->
                    Some (Token.Kwd c)
        )
    in
    Stream.from (fun _ -> lex_stream ())
