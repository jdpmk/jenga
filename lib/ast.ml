type primitive_type = TInt | TChar | TString | TBool
type compound_type = TArr of (primitive_type * int)
type command_type = TPrimitive of primitive_type | TCompound of compound_type

type primitive_value =
  | Int of int
  | Char of char
  | String of string
  | Bool of bool
  | Identifier of string

type compound_value = Array of (primitive_value array * int)
type value = Primitive of primitive_value | Compound of compound_value

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

type memory_op = Read | Write

type command =
  | Value of value
  | UnaryOp of unary_op
  | BinaryOp of binary_op
  | MemoryOp of memory_op
  | IfElse of (block * block * block)
  | While of (block * block)

and block = Block of command list

type alloc = Alloc of (string * command_type * value)
type memory = Memory of alloc list
type program = Program of (memory * block)

let string_of_command c =
  match c with
  | Value (Primitive (Int i)) -> string_of_int i
  | Value (Primitive (Char c)) -> String.make 1 c
  | Value (Primitive (String s)) -> "\"" ^ s ^ "\""
  | Value (Primitive (Bool b)) -> string_of_bool b
  | Value (Primitive (Identifier i)) -> i
  | Value (Compound (Array _)) -> "TODO: unimplemented"
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
  | MemoryOp Read -> "->"
  | MemoryOp Write -> "<-"
