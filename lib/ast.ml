type value = Int of int | String of string | Bool of bool

type unary_op =
  | Dup
  | Drop
  | Swap
  | Over
  | Rot
  | Print
  | Println
  | Eprint
  | Eprintln

type binary_op =
  | Plus
  | Minus
  | Times
  | Divide
  | Mod
  | Exp
  | Eq
  | Neq
  | Lt
  | Leq
  | Gt
  | Geq
  | Land
  | Lor
  | Lnot

type command =
  | Value of value
  | UnaryOp of unary_op
  | BinaryOp of binary_op
  | IfElse of (block * block * block)
  | While of (block * block)

and block = Block of command list

type program = Program of block

let string_of_command c =
  match c with
  | Value (Int i) -> string_of_int i
  | Value (String s) -> "\"" ^ s ^ "\""
  | Value (Bool b) -> string_of_bool b
  | UnaryOp Dup -> "dup"
  | UnaryOp Drop -> "drop"
  | UnaryOp Swap -> "swap"
  | UnaryOp Over -> "over"
  | UnaryOp Rot -> "rot"
  | UnaryOp Print -> "print"
  | UnaryOp Println -> "println"
  | UnaryOp Eprint -> "eprint"
  | UnaryOp Eprintln -> "eprintln"
  | BinaryOp Plus -> "+"
  | BinaryOp Minus -> "-"
  | BinaryOp Times -> "*"
  | BinaryOp Divide -> "/"
  | BinaryOp Mod -> "%"
  | BinaryOp Exp -> "**"
  | BinaryOp Eq -> "="
  | BinaryOp Neq -> "=/="
  | BinaryOp Lt -> "<"
  | BinaryOp Leq -> "<="
  | BinaryOp Gt -> ">"
  | BinaryOp Geq -> ">="
  | BinaryOp Land -> "&&"
  | BinaryOp Lor -> "||"
  | BinaryOp Lnot -> "!"
  (* TODO: implement better pretty printing for IfElse, While *)
  | IfElse _ -> "if"
  | While _ -> "while"
