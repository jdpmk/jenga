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
