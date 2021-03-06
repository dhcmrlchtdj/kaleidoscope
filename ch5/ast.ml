type expr =
    | Number of float
    | Variable of string
    | Binary of char * expr * expr
    | Call of string * expr array
    | If of expr * expr * expr
    | For of string * expr * expr * expr option * expr
and proto = Prototype of string * string array
and func = Function of proto * expr

let rec print_expr = function
    | Number f ->
        print_string "(";
        print_string "number: ";
        print_float f;
        print_string ")";
    | Variable s ->
        print_string "(";
        print_string "variable: ";
        print_string s;
        print_string ")";
    | Binary (op, e1, e2) ->
        print_string "(";
        print_string "binary: ";
        print_char op;
        print_string " ";
        print_expr e1;
        print_string " ";
        print_expr e2;
        print_string ")";
    | Call (fn, args) ->
        print_string "(";
        print_string "func: ";
        print_string fn;
        Array.iter (fun e -> print_string " "; print_expr e) args;
        print_string ")";
    | If (c, t, e) ->
        print_string "(";
        print_string "IF ";
        print_expr c;
        print_string " THEN ";
        print_expr t;
        print_string " ELSE ";
        print_expr e;
        print_string ")";
    | For (id, start, stop, step, body) ->
        print_string "(";
        print_string "FOR ";
        print_string id;
        print_string " = ";
        print_expr start;
        print_string ", ";
        print_expr stop;
        (match step with
            | Some s ->
                print_string ", ";
                print_expr s
            | None -> ());
        print_string " IN ";
        print_expr body;
        print_string ")";

