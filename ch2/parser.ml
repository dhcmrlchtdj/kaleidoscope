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

and parse_kwd c token_stream =
    let _ = parse_whitespace token_stream in
    match Stream.peek token_stream with
        | Some (Token.Kwd x) when x = c ->
            Stream.junk token_stream;
            c
        | _ -> failwith "unexpected"

and parse_def token_stream =
    let _ = parse_whitespace token_stream in
    match Stream.peek token_stream with
        | Some Token.Def ->
            Stream.junk token_stream
        | _ -> failwith "unexpected"

and parse_ext token_stream =
    let _ = parse_whitespace token_stream in
    match Stream.peek token_stream with
        | Some Token.Extern ->
            Stream.junk token_stream
        | _ -> failwith "unexpected"

and parse_ident token_stream =
    let _ = parse_whitespace token_stream in
    match Stream.peek token_stream with
        | Some (Token.Ident x) ->
            Stream.junk token_stream;
            x
        | _ -> failwith "unexpected"

and parse_idents ids token_stream =
    let _ = parse_whitespace token_stream in
    match Stream.peek token_stream with
        | Some (Token.Ident x) ->
            Stream.junk token_stream;
            parse_idents (x :: ids) token_stream
        | _ ->
            List.rev ids

and parse_express token_stream =
    let _ = parse_whitespace token_stream in
    Stream.junk token_stream;
    (* TODO *)
    Ast.Variable "c"

(* prototype ::= id '(' id* ')' *)
(* example: add(x y) *)
and parse_prototype token_stream =
    let id = parse_ident token_stream in
    let _ = parse_kwd '(' token_stream in
    let args = parse_idents [] token_stream in
    let _ = parse_kwd ')' token_stream in
    Ast.Prototype (id, Array.of_list args)

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
    let expr = parse_express token_stream in
    Ast.Function (proto, expr)

(* toplevelexpr ::= expression *)
(* example: x+y *)
and parse_toplevel token_stream =
    let expr = parse_express token_stream in
    Ast.Function (Ast.Prototype ("", [||]), expr)
