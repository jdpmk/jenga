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
%token <char> CHAR
%token <string> STRING
%token TRUE
%token FALSE
%token <string> IDENTIFIER

(* Types *)
%token TINT
%token TCHAR
%token TSTRING
%token TBOOL
%token LBRACKET
%token RBRACKET

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
%token EPRINT
%token EPRINTLN

(* Control flow *)
%token IF
%token THEN
%token ELSE
%token WHILE

(* Memory *)
%token ALLOC
%token AS

(* Delimiters *)
%token DO
%token END

(* Miscellaneous *)
%token EOF

%start <program> parse_program

%%

parse_program:
  | memory=parse_memory main_block=parse_block EOF { Program (memory, main_block) }

parse_memory:
  | allocations=list(parse_alloc) { Memory allocations }

parse_type:
  | TINT { TPrimitive TInt }
  | TCHAR { TPrimitive TChar }
  | TSTRING { TPrimitive TString }
  | TBOOL { TPrimitive TBool }
  | TINT LBRACKET size=INT RBRACKET { TCompound (TArr (TInt, size)) }
  | TCHAR LBRACKET size=INT RBRACKET { TCompound (TArr (TChar, size)) }
  | TSTRING LBRACKET size=INT RBRACKET { TCompound (TArr (TString, size)) }
  | TBOOL LBRACKET size=INT RBRACKET { TCompound (TArr (TBool, size)) }

parse_init:
  | i=INT { Primitive (Int i) }
  | c=CHAR { Primitive (Char c) }
  | s=STRING { Primitive (String s) }
  | TRUE { Primitive (Bool true) }
  | FALSE { Primitive (Bool false) }

parse_alloc:
  | ALLOC i=IDENTIFIER AS t=parse_type d=parse_init END { Alloc (i, t, d) }

parse_block:
  | commands=list(parse_command) { Block commands }

parse_command:
  | DUP { UnaryOp Dup }
  | DROP { UnaryOp Drop }
  | SWAP { UnaryOp Swap }
  | OVER { UnaryOp Over }
  | ROT { UnaryOp Rot }
  | i=INT { Value (Primitive (Int i)) }
  | c=CHAR { Value (Primitive (Char c)) }
  | s=STRING { Value (Primitive (String s)) }
  | i=IDENTIFIER { Value (Primitive (Identifier i)) }
  | TRUE { Value (Primitive (Bool true)) }
  | FALSE { Value (Primitive (Bool false)) }
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
  | EPRINT { UnaryOp Eprint }
  | EPRINTLN { UnaryOp Eprintln }
  | IF condition=parse_block THEN if_body=parse_block ELSE else_body=parse_block END { IfElse (condition, if_body, else_body) }
  | WHILE condition=parse_block DO body=parse_block END { While (condition, body) }
