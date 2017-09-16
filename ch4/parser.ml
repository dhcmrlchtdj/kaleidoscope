let binop_precedence:(char, int) Hashtbl.t =
    Hashtbl.create 10

let precedence c =
    try
        Hashtbl.find binop_precedence c
    with
        | Not_found -> -1

let _ =
    Hashtbl.add binop_precedence '<' 10;
    Hashtbl.add binop_precedence '+' 20;
    Hashtbl.add binop_precedence '-' 20;
    Hashtbl.add binop_precedence '*' 40

let rec parse_whitespace token_stream =
    match Stream.peek token_stream with
        | Some (Token.Whitespace _) ->
            Stream.junk token_stream;
            parse_whitespace token_stream
        | _ -> None

and peek token_stream =
    let _ = parse_whitespace token_stream in
    Stream.peek token_stream

and parse_kwd c token_stream =
    match peek token_stream with
        | Some (Token.Kwd x) when x = c ->
            Stream.junk token_stream;
            c
        | _ -> failwith "unexpected"

and parse_def token_stream =
    match peek token_stream with
        | Some Token.Def ->
            Stream.junk token_stream
        | _ -> failwith "unexpected"

and parse_ext token_stream =
    match peek token_stream with
        | Some Token.Extern ->
            Stream.junk token_stream
        | _ -> failwith "unexpected"

and parse_ident token_stream =
    match peek token_stream with
        | Some (Token.Ident x) ->
            Stream.junk token_stream;
            x
        | _ -> failwith "unexpected"

and parse_idents ids token_stream =
    match peek token_stream with
        | Some (Token.Ident x) ->
            Stream.junk token_stream;
            parse_idents (x :: ids) token_stream
        | _ ->
            List.rev ids

and parse_args args token_stream =
    let _ = parse_whitespace token_stream in
    try
        let arg = parse_expr token_stream in
        let new_args = arg :: args in
        match peek token_stream with
            | Some (Token.Kwd ',') ->
                let _ = parse_kwd ',' token_stream in
                parse_args new_args token_stream
            | _ -> List.rev new_args
    with
        | Failure _ ->
            List.rev args

(* primary ::= numberexpr ::= parenexpr ::= identifierexpr *)
and parse_primary token_stream =
    match peek token_stream with
        (* numberexpr ::= number *)
        | Some (Token.Number f) ->
            Stream.junk token_stream;
            Ast.Number f
        (* parenexpr ::= '(' expression ')' *)
        | Some (Token.Kwd '(') ->
            let _ = parse_kwd '(' token_stream in
            let e = parse_expr token_stream in
            let _ = parse_kwd ')' token_stream in
            e
        (* identifierexpr *)
        (* ::= identifier *)
        (* ::= identifier '(' argumentexpr ')' *)
        | Some (Token.Ident id) ->
            Stream.junk token_stream;
            (match peek token_stream with
                | Some (Token.Kwd '(') ->
                    let _ = parse_kwd '(' token_stream in
                    let args = parse_args [] token_stream in
                    let _ = parse_kwd ')' token_stream in
                    Ast.Call (id, Array.of_list args)
                | _ ->
                    Ast.Variable id)
        | _ -> failwith "unexpected"

(* binoprhs ::= ('+' primary)* *)
and parse_bin_rhs expr_prec lhs token_stream =
    match peek token_stream with
        | Some (Token.Kwd c) when Hashtbl.mem binop_precedence c ->
            let token_prec = precedence c in
            if token_prec < expr_prec
            then lhs
            else (
                Stream.junk token_stream;
                let rhs = parse_primary token_stream in
                let rhs2 = (
                    match peek token_stream with
                        | Some (Token.Kwd c2) ->
                            let next_prec = precedence c2 in
                            if token_prec < next_prec
                            then parse_bin_rhs (token_prec + 1) rhs token_stream
                            else rhs
                        | _ -> rhs
                ) in
                let lhs2 = Ast.Binary (c, lhs, rhs2) in
                parse_bin_rhs expr_prec lhs2 token_stream
            )
        | _ -> lhs

(* expression ::= primary binoprhs *)
and parse_expr token_stream =
    let lhs = parse_primary token_stream in
    parse_bin_rhs 0 lhs token_stream

(* prototype ::= id '(' id* ')' *)
(* example: add(x y) *)
and parse_prototype token_stream =
    let id = parse_ident token_stream in
    let _ = parse_kwd '(' token_stream in
    let params = parse_idents [] token_stream in
    let _ = parse_kwd ')' token_stream in
    Ast.Prototype (id, Array.of_list params)

(* external ::= 'extern' prototype *)
(* example: extern add(x y) *)
and parse_extern token_stream =
    let _ = parse_ext token_stream in
    let proto = parse_prototype token_stream in
    proto

(* definition ::= 'def' prototype expression *)
(* example: def add(x y) x+y *)
and parse_definition token_stream =
    let _ = parse_def token_stream in
    let proto = parse_prototype token_stream in
    let expr = parse_expr token_stream in
    Ast.Function (proto, expr)

(* toplevelexpr ::= expression *)
(* example: x+y *)
and parse_toplevel token_stream =
    let expr = parse_expr token_stream in
    Ast.Function (Ast.Prototype ("", [||]), expr)
