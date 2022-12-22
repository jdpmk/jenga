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

(* Miscellaneous *)
%token EOF

%start <program> parse_program

%%

parse_program:
  | program_tokens=list(program_token) EOF { Program program_tokens }

program_token:
  | DUP { UnaryOp Dup }
  | DROP { UnaryOp Drop }
  | SWAP { UnaryOp Swap }
  | OVER { UnaryOp Over }
  | ROT { UnaryOp Over }
  | i=INT { Const (Int i) }
  | s=STRING { Const (String s) }
  | TRUE { Const (Bool true) }
  | FALSE { Const (Bool false) }
  | PLUS { BinaryOp Plus }
  | MINUS { BinaryOp Minus }
  | TIMES { BinaryOp Times }
  | DIVIDE { BinaryOp Divide }
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
