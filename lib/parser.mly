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

(* TODO: return a type representing a complete program *)
%start <stack_op> main

%%

(* TODO: parse more language constructs *)
main:
  DUP { Dup }
