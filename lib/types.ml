open Ast
open Exceptions

let rec string_of_command_type t =
  match t with
  | TPrimitive TInt -> "int"
  | TPrimitive TChar -> "char"
  | TPrimitive TString -> "string"
  | TPrimitive TBool -> "bool"
  | TCompound (TArr tt) -> string_of_command_type (TPrimitive tt) ^ "[]"

(* TODO: include token position in error message *)
let rec type_check_command (type_stack : command_type list) (c : command) =
  let command_string = string_of_command c in
  match c with
  | Value (Primitive (Int _)) -> TPrimitive TInt :: type_stack
  | Value (Primitive (Char _)) -> TPrimitive TChar :: type_stack
  | Value (Primitive (String _)) -> TPrimitive TString :: type_stack
  | Value (Primitive (Bool _)) -> TPrimitive TBool :: type_stack
  | Value (Primitive (Identifier _)) -> raise (Failure "unimplemented")
  | Value (Compound (Array _)) -> raise (Failure "unimplemented")
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
          if a = TPrimitive TInt && b = TPrimitive TInt then
            TPrimitive TInt :: rest
          else
            raise
              (TypeError
                 ("cannot execute `" ^ command_string ^ "`. expected `"
                 ^ string_of_command_type (TPrimitive TInt)
                 ^ "` and `"
                 ^ string_of_command_type (TPrimitive TInt)
                 ^ "` but found `" ^ string_of_command_type a ^ "` and `"
                 ^ string_of_command_type b ^ "`"))
      | _ ->
          raise
            (TypeError
               ("cannot execute `" ^ command_string
              ^ "`. expected two items of type `"
               ^ string_of_command_type (TPrimitive TInt)
               ^ "` on the stack but found one item or none")))
  | BinaryOp Eq
  | BinaryOp Neq
  | BinaryOp Lt
  | BinaryOp Leq
  | BinaryOp Gt
  | BinaryOp Geq -> (
      match type_stack with
      | b :: a :: rest ->
          if a = b then TPrimitive TBool :: rest
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
          if a = TPrimitive TBool && b = TPrimitive TBool then
            TPrimitive TBool :: rest
          else
            raise
              (TypeError
                 ("cannot execute `" ^ command_string
                ^ "`. expected two items of type `"
                 ^ string_of_command_type (TPrimitive TBool)
                 ^ "` and `"
                 ^ string_of_command_type (TPrimitive TBool)
                 ^ "` but found `" ^ string_of_command_type a ^ "` and `"
                 ^ string_of_command_type b))
      | _ ->
          raise
            (TypeError
               ("cannot execute `" ^ command_string
              ^ "`. expected two items of type `"
               ^ string_of_command_type (TPrimitive TBool)
               ^ "` on the stack but found one item or none")))
  | BinaryOp Lnot -> (
      match type_stack with
      | a :: rest ->
          if a = TPrimitive TBool then TPrimitive TBool :: rest
          else
            raise
              (TypeError
                 ("cannot execute `" ^ command_string ^ "`. expected `"
                 ^ string_of_command_type (TPrimitive TBool)
                 ^ "` but found `" ^ string_of_command_type a))
      | _ ->
          raise
            (TypeError
               ("cannot execute `" ^ command_string
              ^ "`. expected one item of type `"
               ^ string_of_command_type (TPrimitive TBool)
               ^ "` on the stack but found none")))
  | IfElse (condition, if_body, else_body) -> (
      match type_check_block condition type_stack with
      | a :: rest ->
          if a = TPrimitive TBool then
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
          if a = TPrimitive TBool then
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
