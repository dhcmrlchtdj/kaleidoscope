let lex chars =
    let map_chars f =
        match Stream.peek chars with
            | None -> None
            | Some x -> f x
    in
    let rec lex_ident buffer =
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
    and lex_stream () =
        map_chars (function
            | ' ' | '\n' | '\r' | '\t' ->
                Stream.junk chars;
                lex_stream ()
            | 'A'..'Z' | 'a'..'z' as c ->
                Stream.junk chars;
                let buffer = Buffer.create 1 in
                Buffer.add_char buffer c;
                lex_ident buffer
            | '0'..'9' as c ->
                Stream.junk chars;
                let buffer = Buffer.create 1 in
                Buffer.add_char buffer c;
                lex_number buffer
            | '#' as c ->
                Stream.junk chars;
                let buffer = Buffer.create 1 in
                Buffer.add_char buffer c;
                lex_comment buffer
            | c ->
                Some (Token.Kwd c)
        )
    in
    Stream.from (fun i -> lex_stream ())
