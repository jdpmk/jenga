{
  open Parser
}

let whitespace   = [' ' '\t']+
let alpha        = ['a' - 'z' 'A' - 'Z']
let digit        = ['0' - '9']
let alphanumeric = (alpha | digit)+
let integer      = '-'? digit+

(* TODO: add more rules to parse tokens *)
rule token = parse
  | whitespace            { token lexbuf }
  | "dup"                 { DUP }
  | "drop"                { DROP }
  | "swap"                { SWAP }
  | "over"                { OVER }
  | "rot"                 { ROT }
  | integer               { INT (int_of_string (Lexing.lexeme lexbuf)) }
  (* TODO: handle escape sequences *)
  | '"' alphanumeric* '"' { STRING (Lexing.lexeme lexbuf) }
  | "true"                { TRUE }
  | "false"               { FALSE }
  | "="                   { EQ }
  | "!="                  { NEQ }
  | "<"                   { LT }
  | "<="                  { LEQ }
  | ">"                   { GT }
  | ">="                  { GEQ }
  | "&&"                  { LAND }
  | "||"                  { LOR }
  | "!"                   { LNOT }
  | "print"               { PRINT }
  | "println"             { PRINTLN }
