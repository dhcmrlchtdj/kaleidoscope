type token =
    | Def
    | Extern
    | Ident of string
    | Whitespace of string
    | Comment of string
    | Number of float
    | Kwd of char
    | If
    | Then
    | Else
    | For
    | In

let print_token = function
    | Def -> print_endline "Def"
    | Extern -> print_endline "Extern"
    | Ident s -> print_endline ("Ident: " ^ s)
    | Whitespace s -> print_endline "Whitespace"
    | Comment s -> print_endline ("Comment: " ^ s)
    | Number f -> print_endline ("Number: " ^ (string_of_float f))
    | Kwd c -> print_endline ("Kwd: " ^ (Char.escaped c))
    | If -> print_endline "If"
    | Then -> print_endline "Then"
    | Else -> print_endline "Else"
    | For -> print_endline "For"
    | In -> print_endline "In"
