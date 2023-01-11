open Ast
open Exceptions

type primitive_type = TInt | TChar | TString | TBool
type compound_type = TArr of primitive_type
type command_type = Primitive of primitive_type | Compound of compound_type

let rec string_of_command_type t =
  match t with
  | Primitive TInt -> "int"
  | Primitive TChar -> "char"
  | Primitive TString -> "string"
  | Primitive TBool -> "bool"
  | Compound (TArr tt) -> string_of_command_type (Primitive tt) ^ "[]"

(* TODO: include token position in error message *)
let rec type_check_command (type_stack : command_type list) (c : command) =
  let command_string = string_of_command c in
  match c with
  | Value (Int _) -> Primitive TInt :: type_stack
  | Value (Char _) -> Primitive TChar :: type_stack
  | Value (String _) -> Primitive TString :: type_stack
  | Value (Bool _) -> Primitive TBool :: type_stack
  | Value (Identifier _) -> raise (Failure "unimplemented")
  | UnaryOp Dup -> (
      match type_stack with
      | a :: rest -> a :: a :: rest
      | _ ->
          raise
            (TypeError
               ("cannot execute `" ^ command_string
              ^ "`. expected one item on the stack but found none")))
  | UnaryOp Drop
  | UnaryOp Print
  | UnaryOp Println
  | UnaryOp Eprint
  | UnaryOp Eprintln -> (
      match type_stack with
      | _ :: rest -> rest
      | _ ->
          raise
            (TypeError
               ("cannot execute `" ^ command_string
              ^ "`. expected one item on the stack but found none")))
  | UnaryOp Swap -> (
      match type_stack with
      | b :: a :: rest -> a :: b :: rest
      | _ ->
          raise
            (TypeError
               ("cannot execute `" ^ command_string
              ^ "`. expected two items on the stack but found one item or none"
               )))
  | UnaryOp Over -> (
      match type_stack with
      | b :: a :: rest -> a :: b :: a :: rest
      | _ ->
          raise
            (TypeError
               ("cannot execute `" ^ command_string
              ^ "`. expected two items on the stack but found one item or none"
               )))
  | UnaryOp Rot -> (
      match type_stack with
      | c :: b :: a :: rest -> a :: c :: b :: rest
      | _ ->
          raise
            (TypeError
               ("cannot execute `" ^ command_string
              ^ "`. expected three items on the stack but found one item, two \
                 items, or none")))
  | BinaryOp Plus
  | BinaryOp Minus
  | BinaryOp Times
  | BinaryOp Divide
  | BinaryOp Mod
  | BinaryOp Exp -> (
      match type_stack with
      | b :: a :: rest ->
          if a = Primitive TInt && b = Primitive TInt then Primitive TInt :: rest
          else
            raise
              (TypeError
                 ("cannot execute `" ^ command_string ^ "`. expected `"
                 ^ string_of_command_type (Primitive TInt)
                 ^ "` and `"
                 ^ string_of_command_type (Primitive TInt)
                 ^ "` but found `" ^ string_of_command_type a ^ "` and `"
                 ^ string_of_command_type b ^ "`"))
      | _ ->
          raise
            (TypeError
               ("cannot execute `" ^ command_string
              ^ "`. expected two items of type `"
               ^ string_of_command_type (Primitive TInt)
               ^ "` on the stack but found one item or none")))
  | BinaryOp Eq
  | BinaryOp Neq
  | BinaryOp Lt
  | BinaryOp Leq
  | BinaryOp Gt
  | BinaryOp Geq -> (
      match type_stack with
      | b :: a :: rest ->
          if a = b then Primitive TBool :: rest
          else
            raise
              (TypeError
                 ("cannot execute `" ^ command_string
                ^ "`. expected two items with the same type but found `"
                ^ string_of_command_type a ^ "` and `"
                ^ string_of_command_type b))
      | _ ->
          raise
            (TypeError
               ("cannot execute `" ^ command_string
              ^ "`. expected two items with the same type on the stack but \
                 found one item or none")))
  | BinaryOp Land | BinaryOp Lor -> (
      match type_stack with
      | b :: a :: rest ->
          if a = Primitive TBool && b = Primitive TBool then Primitive TBool :: rest
          else
            raise
              (TypeError
                 ("cannot execute `" ^ command_string
                ^ "`. expected two items of type `"
                 ^ string_of_command_type (Primitive TBool)
                 ^ "` and `"
                 ^ string_of_command_type (Primitive TBool)
                 ^ "` but found `" ^ string_of_command_type a ^ "` and `"
                 ^ string_of_command_type b))
      | _ ->
          raise
            (TypeError
               ("cannot execute `" ^ command_string
              ^ "`. expected two items of type `"
               ^ string_of_command_type (Primitive TBool)
               ^ "` on the stack but found one item or none")))
  | BinaryOp Lnot -> (
      match type_stack with
      | a :: rest ->
          if a = Primitive TBool then Primitive TBool :: rest
          else
            raise
              (TypeError
                 ("cannot execute `" ^ command_string ^ "`. expected `"
                 ^ string_of_command_type (Primitive TBool)
                 ^ "` but found `" ^ string_of_command_type a))
      | _ ->
          raise
            (TypeError
               ("cannot execute `" ^ command_string
              ^ "`. expected one item of type `"
               ^ string_of_command_type (Primitive TBool)
               ^ "` on the stack but found none")))
  | IfElse (condition, if_body, else_body) -> (
      match type_check_block condition type_stack with
      | a :: rest ->
          if a = Primitive TBool then
            if rest = type_stack then
              if type_check_block if_body type_stack = type_stack then
                if type_check_block else_body type_stack = type_stack then
                  type_stack
                else
                  raise
                    (TypeError
                       "else body must not modify the structure of the stack")
              else
                raise
                  (TypeError
                     "if body must not modify the structure of the stack")
            else
              raise
                (TypeError
                   "if-else condition must not modify the structure of the \
                    stack")
          else raise (TypeError "if-else condition must produce a `bool`")
      | _ ->
          raise
            (TypeError
               "if-else condition must not modify the structure of the stack"))
  | While (condition, body) -> (
      match type_check_block condition type_stack with
      | a :: rest ->
          if a = Primitive TBool then
            if rest = type_stack then
              if type_check_block body type_stack = type_stack then type_stack
              else
                raise
                  (TypeError
                     "while loop body must not modify the structure of the \
                      stack")
            else
              raise
                (TypeError
                   "while loop condition must not modify the structure of the \
                    stack")
          else raise (TypeError "while loop condition must produce a `bool`")
      | _ ->
          raise
            (TypeError
               "while loop condition must produce a `bool` and cannot modify \
                the structure of the stack"))

and type_check_block (b : block) (initial_stack : command_type list) :
    command_type list =
  match b with
  | Block commands -> List.fold_left type_check_command initial_stack commands

let type_check_program (p : program) : unit =
  match p with
  | Program b ->
      let terminal_stack = type_check_block b [] in
      if List.length terminal_stack = 0 then ()
      else
        raise
          (TypeError
             (string_of_int (List.length terminal_stack)
             ^ " unhandled item(s) on the stack"))
