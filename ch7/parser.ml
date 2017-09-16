let binop_precedence:(char, int) Hashtbl.t =
    Hashtbl.create 10

let precedence c =
    try
        Hashtbl.find binop_precedence c
    with
        | Not_found -> -1

let _ =
    Hashtbl.add binop_precedence '=' 2;
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

and consume_token t token_stream =
    match peek token_stream with
        | Some x when x = t ->
            Stream.junk token_stream
        | _ -> failwith "unexpected"

and parse_kwd token_stream =
    match peek token_stream with
        | Some (Token.Kwd c) ->
            Stream.junk token_stream;
            c
        | _ -> failwith "unexpected"

and parse_number token_stream =
    match peek token_stream with
        | Some (Token.Number f) ->
            Stream.junk token_stream;
            f
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
    try
        let arg = parse_expr token_stream in
        let new_args = arg :: args in
        match peek token_stream with
            | Some (Token.Kwd ',') ->
                consume_token (Token.Kwd ',') token_stream;
                parse_args new_args token_stream
            | _ -> List.rev new_args
    with
        | Failure _ ->
            List.rev args

and parse_var_init vars token_stream =
    let id = parse_ident token_stream in
    let exp =
        match peek token_stream with
            | Some (Token.Kwd '=') ->
                consume_token (Token.Kwd '=') token_stream;
                Some (parse_expr token_stream)
            | _ ->
                None
    in
    let vars2 = (id, exp) :: vars in
    match peek token_stream with
        | Some (Token.Kwd ',') ->
            consume_token (Token.Kwd ',') token_stream;
            parse_var_init vars2 token_stream
        | _ ->
            List.rev vars2

(* primary ::= numberexpr ::= parenexpr ::= identifierexpr ::= ifexpr ::= forexpr ::= varexpr *)
and parse_primary token_stream =
    match peek token_stream with
        (* numberexpr ::= number *)
        | Some (Token.Number f) ->
            Stream.junk token_stream;
            Ast.Number f
        (* parenexpr ::= '(' expression ')' *)
        | Some (Token.Kwd '(') ->
            consume_token (Token.Kwd '(') token_stream;
            let e = parse_expr token_stream in
            consume_token (Token.Kwd ')') token_stream;
            e
        (* identifierexpr *)
        (* ::= identifier *)
        (* ::= identifier '(' argumentexpr ')' *)
        | Some (Token.Ident id) ->
            Stream.junk token_stream;
            (match peek token_stream with
                | Some (Token.Kwd '(') ->
                    consume_token (Token.Kwd '(') token_stream;
                    let args = parse_args [] token_stream in
                    consume_token (Token.Kwd ')') token_stream;
                    Ast.Call (id, Array.of_list args)
                | _ ->
                    Ast.Variable id)
        (* ifexpr ::= 'if' expr 'then' expr 'else' expr *)
        | Some Token.If ->
            consume_token Token.If token_stream;
            let cond = parse_expr token_stream in
            consume_token Token.Then token_stream;
            let then_ = parse_expr token_stream in
            consume_token Token.Else token_stream;
            let else_ = parse_expr token_stream in
            Ast.If (cond, then_, else_)
        (* forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression *)
        | Some Token.For ->
            consume_token Token.For token_stream;
            let id = parse_ident token_stream in
            consume_token (Token.Kwd '=') token_stream;
            let start = parse_expr token_stream in
            consume_token (Token.Kwd ',') token_stream;
            let stop = parse_expr token_stream in
            let step = (
                match peek token_stream with
                    | Some (Token.Kwd ',') ->
                        consume_token (Token.Kwd ',') token_stream;
                        Some (parse_expr token_stream)
                    | _ -> None
            ) in
            consume_token (Token.In) token_stream;
            let body = parse_expr token_stream in
            Ast.For (id, start, stop, step, body)
        (* varexpr ::= 'var' identifier ('=' expression (',' identifier ('=' expression)?)* )? 'in' expression *)
        | Some Token.Var ->
            consume_token Token.Var token_stream;
            let vars = parse_var_init [] token_stream in
            consume_token (Token.In) token_stream;
            let body = parse_expr token_stream in
            Ast.Var (Array.of_list vars, body)
        | _ -> failwith "unexpected"

(* unary ::= primary ::= '!' unary *)
and parse_unary token_stream =
    match peek token_stream with
        | Some (Token.Kwd op) when (op != '(' && op != '=') ->
            consume_token (Token.Kwd op) token_stream;
            let operand = parse_expr token_stream in
            Ast.Unary (op, operand)
        | _ -> parse_primary token_stream

(* binoprhs ::= ('+' primary)* *)
and parse_bin_rhs expr_prec lhs token_stream =
    match peek token_stream with
        | Some (Token.Kwd c) when Hashtbl.mem binop_precedence c ->
            let token_prec = precedence c in
            if token_prec < expr_prec
            then lhs
            else (
                Stream.junk token_stream;
                let rhs = parse_unary token_stream in
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
    let lhs = parse_unary token_stream in
    parse_bin_rhs 0 lhs token_stream

(* prototype
 * ::= binary LETTER number? (id, id)
 * ::= unary LETTER (id)
 * ::= id '(' id* ')'
 * *)
(* example: add(x y) *)
and parse_prototype token_stream = (
    match peek token_stream with
        | Some Token.Binary ->
            consume_token Token.Binary token_stream;
            let c = parse_kwd token_stream in
            let name = "binary" ^ (Char.escaped c) in
            let f = parse_number token_stream in
            let prec = int_of_float f in
            consume_token (Token.Kwd '(') token_stream;
            let left = parse_ident token_stream in
            let right = parse_ident token_stream in
            consume_token (Token.Kwd ')') token_stream;
            Ast.BinOpPrototype (name, [|left; right|], prec)
        | Some Token.Unary ->
            consume_token Token.Unary token_stream;
            let c = parse_kwd token_stream in
            let name = "unary" ^ (Char.escaped c) in
            consume_token (Token.Kwd '(') token_stream;
            let arg = parse_ident token_stream in
            consume_token (Token.Kwd ')') token_stream;
            Ast.Prototype (name, [|arg|])
        | Some Token.Ident id ->
            consume_token (Token.Ident id) token_stream;
            consume_token (Token.Kwd '(') token_stream;
            let params = parse_idents [] token_stream in
            consume_token (Token.Kwd ')') token_stream;
            Ast.Prototype (id, Array.of_list params)
        | _ ->
            failwith "unexpected"
)

(* external ::= 'extern' prototype *)
(* example: extern add(x y) *)
and parse_extern token_stream =
    consume_token Token.Extern token_stream;
    let proto = parse_prototype token_stream in
    proto

(* definition ::= 'def' prototype expression *)
(* example: def add(x y) x+y *)
and parse_definition token_stream =
    consume_token Token.Def token_stream;
    let proto = parse_prototype token_stream in
    let expr = parse_expr token_stream in
    Ast.Function (proto, expr)

(* toplevelexpr ::= expression *)
(* example: x+y *)
and parse_toplevel token_stream =
    let expr = parse_expr token_stream in
    Ast.Function (Ast.Prototype ("", [||]), expr)
