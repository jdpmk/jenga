%{
  open Ast
%}

(* Stack manipulation operators *)
%token DUP
%token DROP
%token SWAP
%token OVER
%token ROT

(* Literals *)
%token <int> INT
%token <string> STRING
%token TRUE
%token FALSE

(* Arithmetic operators *)
%token PLUS
%token MINUS
%token TIMES
%token DIVIDE
%token MOD
%token EXP

(* Comparison operators *)
%token EQ
%token NEQ
%token LT
%token LEQ
%token GT
%token GEQ

(* Logical operators *)
%token LAND
%token LOR
%token LNOT

(* I/O functions *)
%token PRINT
%token PRINTLN

(* Control flow *)
%token IF
%token THEN
%token ELSE
%token WHILE
%token DO
%token END

(* Miscellaneous *)
%token EOF

%start <program> parse_program

%%

parse_program:
  | main_block=parse_block EOF { Program main_block }

parse_block:
  | commands=list(parse_command) { Block commands }

parse_command:
  | DUP { UnaryOp Dup }
  | DROP { UnaryOp Drop }
  | SWAP { UnaryOp Swap }
  | OVER { UnaryOp Over }
  | ROT { UnaryOp Rot }
  | i=INT { Const (Int i) }
  | s=STRING { Const (String s) }
  | TRUE { Const (Bool true) }
  | FALSE { Const (Bool false) }
  | PLUS { BinaryOp Plus }
  | MINUS { BinaryOp Minus }
  | TIMES { BinaryOp Times }
  | DIVIDE { BinaryOp Divide }
  | MOD { BinaryOp Mod }
  | EXP { BinaryOp Exp }
  | EQ { BinaryOp Eq }
  | NEQ { BinaryOp Neq }
  | LT { BinaryOp Lt }
  | LEQ { BinaryOp Leq }
  | GT { BinaryOp Gt }
  | GEQ { BinaryOp Geq }
  | LAND { BinaryOp Land }
  | LOR { BinaryOp Lor }
  | LNOT { BinaryOp Lnot }
  | PRINT { UnaryOp Print }
  | PRINTLN { UnaryOp Println }
  | IF condition=parse_block THEN if_body=parse_block ELSE else_body=parse_block END { IfElse (condition, if_body, else_body) }
  | WHILE condition=parse_block DO body=parse_block END { While (condition, body) }
