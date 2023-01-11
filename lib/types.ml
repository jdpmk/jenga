open Ast
open Env
open Exceptions

let rec string_of_command_type t =
  match t with
  | TPrimitive TInt -> "int"
  | TPrimitive TChar -> "char"
  | TPrimitive TString -> "string"
  | TPrimitive TBool -> "bool"
  | TCompound (TArr (tt, _)) -> string_of_command_type (TPrimitive tt) ^ "[]"

let build_memory (m : memory) : command_type environment =
  match m with
  | Memory allocations -> List.map (fun (Alloc (i, t, _)) -> (i, t)) allocations

(* TODO: include token position in error message *)
let rec type_check_command
    ((m, type_stack) : command_type environment * command_type list)
    (c : command) : command_type environment * command_type list =
  let aux c =
    let command_string = string_of_command c in
    match c with
    | Value (Primitive (Int _)) -> TPrimitive TInt :: type_stack
    | Value (Primitive (Char _)) -> TPrimitive TChar :: type_stack
    | Value (Primitive (String _)) -> TPrimitive TString :: type_stack
    | Value (Primitive (Bool _)) -> TPrimitive TBool :: type_stack
    | Value (Primitive (Identifier i)) -> (
        match get m i with
        | None ->
            raise
              (TypeError
                 ("unable to determine type for unknown identifier `" ^ i ^ "`"))
        | Some a -> a :: type_stack)
    | Value (Compound (Array _)) ->
        (* Literal arrays are not handled by the lexer nor the parser *)
        raise (Failure "unreachable")
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
                ^ "`. expected two items on the stack but found one item or \
                   none")))
    | UnaryOp Over -> (
        match type_stack with
        | b :: a :: rest -> a :: b :: a :: rest
        | _ ->
            raise
              (TypeError
                 ("cannot execute `" ^ command_string
                ^ "`. expected two items on the stack but found one item or \
                   none")))
    | UnaryOp Rot -> (
        match type_stack with
        | c :: b :: a :: rest -> a :: c :: b :: rest
        | _ ->
            raise
              (TypeError
                 ("cannot execute `" ^ command_string
                ^ "`. expected three items on the stack but found one item, \
                   two items, or none")))
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
        match type_check_block condition m type_stack with
        | _, a :: rest ->
            if a = TPrimitive TBool then
              if rest = type_stack then
                if type_check_block if_body m type_stack = (m, type_stack) then
                  if type_check_block else_body m type_stack = (m, type_stack)
                  then type_stack
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
                 "if-else condition must not modify the structure of the stack")
        )
    | While (condition, body) -> (
        match type_check_block condition m type_stack with
        | _, a :: rest ->
            if a = TPrimitive TBool then
              if rest = type_stack then
                if type_check_block body m type_stack = (m, type_stack) then
                  type_stack
                else
                  raise
                    (TypeError
                       "while loop body must not modify the structure of the \
                        stack")
              else
                raise
                  (TypeError
                     "while loop condition must not modify the structure of \
                      the stack")
            else raise (TypeError "while loop condition must produce a `bool`")
        | _ ->
            raise
              (TypeError
                 "while loop condition must produce a `bool` and cannot modify \
                  the structure of the stack"))
    | MemoryOp Read -> (
        match type_stack with
        | b :: a :: rest -> (
            match a with
            | TCompound (TArr (t, size)) ->
                if b = TPrimitive TInt then TPrimitive t :: rest
                else
                  raise
                    (TypeError
                       ("cannot execute `" ^ command_string ^ "`. expected `"
                       ^ string_of_command_type (TCompound (TArr (t, size)))
                       ^ "` and `"
                       ^ string_of_command_type (TPrimitive TInt)
                       ^ "` but found `" ^ string_of_command_type a ^ "` and `"
                       ^ string_of_command_type b ^ "`"))
            | _ ->
                raise
                  (TypeError
                     ("cannot execute `" ^ command_string
                    ^ "`. expected array type but the first argument was `"
                    ^ string_of_command_type a ^ "`")))
        | _ ->
            raise
              (TypeError
                 ("cannot execute `" ^ command_string
                ^ "`. expected two items of array type and `"
                 ^ string_of_command_type (TPrimitive TInt)
                 ^ "` but found one item or none")))
    | MemoryOp Write -> (
        match type_stack with
        | c :: b :: a :: rest -> (
            match a with
            | TCompound (TArr (t, size)) ->
                if b = TPrimitive TInt && c = TPrimitive t then
                  rest
                else
                  raise
                    (TypeError
                       ("cannot execute `" ^ command_string ^ "`. expected `"
                       ^ string_of_command_type (TCompound (TArr (t, size)))
                       ^ "`, `"
                       ^ string_of_command_type (TPrimitive TInt)
                       ^ "`, and `"
                       ^ string_of_command_type (TPrimitive t)
                       ^ "` but found `" ^ string_of_command_type a ^ "`, `"
                       ^ string_of_command_type b ^ "`, and `"
                       ^ string_of_command_type c))
            | _ ->
                raise
                  (TypeError
                     ("cannot execute `" ^ command_string
                    ^ "`. expected array type and `"
                     ^ string_of_command_type (TPrimitive TInt)
                     ^ "` but the first argument was `"
                     ^ string_of_command_type a ^ "`")))
        | _ ->
            raise
              (TypeError
                 ("cannot execute `" ^ command_string
                ^ "`. expected three items of array type, `"
                 ^ string_of_command_type (TPrimitive TInt)
                 ^ "`, and a primitive of the array type but found two items, \
                    one item or none")))
  in
  (m, aux c)

and type_check_block (b : block) (m : command_type environment)
    (initial_stack : command_type list) :
    command_type environment * command_type list =
  match b with
  | Block commands ->
      List.fold_left type_check_command (m, initial_stack) commands

let type_check_program (p : program) : unit =
  match p with
  | Program (m, b) ->
      let m = build_memory m in
      let _, terminal_stack = type_check_block b m [] in
      if List.length terminal_stack = 0 then ()
      else
        raise
          (TypeError
             (string_of_int (List.length terminal_stack)
             ^ " unhandled item(s) on the stack"))
