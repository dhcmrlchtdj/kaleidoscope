type token =
    | Def
    | Extern
    | Ident of string
    | Comment of string
    | Number of float
    | Kwd of char

let print_token = function
    | Def -> print_endline "Def"
    | Extern -> print_endline "Extern"
    | Ident s -> print_endline "Ident"
    | Comment s -> print_endline "Comment"
    | Number f -> print_endline "Number"
    | Kwd c -> print_endline "Kwd"
