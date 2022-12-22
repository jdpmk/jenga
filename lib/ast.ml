type const = Int of int | String of string | Bool of bool
type unary_op = Dup | Drop | Swap | Over | Rot | Print | Println

type binary_op =
  | Plus
  | Minus
  | Times
  | Divide
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

type program_token =
  | Const of const
  | UnaryOp of unary_op
  | BinaryOp of binary_op

type program = Program of program_token list

let string_of_program_token token =
  match token with
  | Const (Int i) -> string_of_int i
  | Const (String s) -> "\"" ^ s ^ "\""
  | Const (Bool b) -> string_of_bool b
  | UnaryOp Dup -> "dup"
  | UnaryOp Drop -> "drop"
  | UnaryOp Swap -> "swap"
  | UnaryOp Over -> "over"
  | UnaryOp Rot -> "rot"
  | UnaryOp Print -> "print"
  | UnaryOp Println -> "println"
  | BinaryOp Plus -> "+"
  | BinaryOp Minus -> "-"
  | BinaryOp Times -> "*"
  | BinaryOp Divide -> "/"
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
