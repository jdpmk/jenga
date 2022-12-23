{
  open Exceptions
  open Parser
}

let whitespace = [' ' '\t']+
let alpha = ['a' - 'z' 'A' - 'Z']
let digit = ['0' - '9']
let alphanumeric = (alpha | digit)+
let integer = '-'? digit+

rule read_token = parse
  | whitespace { read_token lexbuf }
  | "#" { read_comment lexbuf }
  | eof { EOF }
  (* TODO: test with multi-line programs *)
  | "\n" { EOF }
  | "dup" { DUP }
  | "drop" { DROP }
  | "swap" { SWAP }
  | "over" { OVER }
  | "rot" { ROT }
  | integer { INT (int_of_string (Lexing.lexeme lexbuf)) }
  (* TODO: handle escape sequences *)
  | '"' ((alphanumeric | whitespace)* as s) '"' { STRING s }
  | "true" { TRUE }
  | "false" { FALSE }
  | "+" { PLUS }
  | "-" { MINUS }
  | "*" { TIMES }
  | "/" { DIVIDE }
  | "**" { EXP }
  | "=" { EQ }
  | "=/=" { NEQ }
  | "<" { LT }
  | "<=" { LEQ }
  | ">" { GT }
  | ">=" { GEQ }
  | "&&" { LAND }
  | "||" { LOR }
  | "!" { LNOT }
  | "print" { PRINT }
  | "println" { PRINTLN }
  | _ { raise (SyntaxError ("unexpected token: " ^ Lexing.lexeme lexbuf)) }
and
  read_comment = parse
  | "\n" { read_token lexbuf }
  | _ { read_comment lexbuf }
