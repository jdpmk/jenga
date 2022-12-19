{
  open Parser
}

let whitespace = [' ' '\t']+
let alpha = ['a' - 'z' 'A' - 'Z']
let digit = ['0' - '9']
let alphanumeric = (alpha | digit)+
let integer = '-'? digit+

(* TODO: handle comments *)
rule read_token = parse
  | whitespace { read_token lexbuf }
  | eof { EOF }
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
