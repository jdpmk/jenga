{
  open Exceptions
  open Parser
}

let whitespace = [' ' '\t' '\n']+
let alpha = ['a' - 'z' 'A' - 'Z']
let digit = ['0' - '9']
let alphanumeric = (alpha | digit)+
let integer = '-'? digit+

rule read_token = parse
  | whitespace { read_token lexbuf }
  | "dup" { DUP }
  | "drop" { DROP }
  | "swap" { SWAP }
  | "over" { OVER }
  | "rot" { ROT }
  | integer { INT (int_of_string (Lexing.lexeme lexbuf)) }
  (* TODO: handle escape sequences and other special characters *)
  | "'" ((alpha | digit) as c) "'" { CHAR c }
  (* TODO: handle escape sequences *)
  | '"' ((alphanumeric | whitespace)* as s) '"' { STRING s }
  | "int" { TINT }
  | "char" { TCHAR }
  | "string" { TSTRING }
  | "bool" { TBOOL }
  | "[" { LBRACKET }
  | "]" { RBRACKET }
  | "true" { TRUE }
  | "false" { FALSE }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIVIDE }
  | "%" { MOD }
  | "**" { EXP }
  | "=" { EQ }
  | "!=" { NEQ }
  | "<" { LT }
  | "<=" { LEQ }
  | ">" { GT }
  | ">=" { GEQ }
  | "&&" { LAND }
  | "||" { LOR }
  | "!" { LNOT }
  | "print" { PRINT }
  | "println" { PRINTLN }
  | "eprint" { EPRINT }
  | "eprintln" { EPRINTLN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | "while" { WHILE }
  | "alloc" { ALLOC }
  | "as" { AS }
  | "do" { DO }
  | "end" { END }
  | (alpha (alpha | digit)*) as s { IDENTIFIER s }
  | "#" { read_comment lexbuf }
  | eof { EOF }
  | _ { raise (SyntaxError ("unexpected token: " ^ Lexing.lexeme lexbuf)) }
and
  read_comment = parse
  | "\n" { read_token lexbuf }
  | _ { read_comment lexbuf }
