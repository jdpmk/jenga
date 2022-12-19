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

(* TODO: return a type representing a complete program *)
%start <stack_op> main

%%

(* TODO: parse more language constructs *)
main:
  DUP { Dup }
