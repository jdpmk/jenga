open Ast
open Exceptions

type token_type =
  | TAny
  | TInt
  | TString
  | TBool
  | TOp of token_type list * token_type list

let rec string_of_token_type t =
  match t with
  | TAny -> "any"
  | TInt -> "int"
  | TString -> "string"
  | TBool -> "bool"
  | TOp (inputs, outputs) ->
      String.concat " -> " (List.map string_of_token_type inputs)
      ^ " -> "
      ^ String.concat " -> " (List.map string_of_token_type outputs)

let type_check_token (type_stack : token_type list) (token : program_token) =
  let token_string = string_of_program_token token in
  match token with
  | Const (Int _) -> TInt :: type_stack
  | Const (String _) -> TString :: type_stack
  | Const (Bool _) -> TBool :: type_stack
  | UnaryOp Dup -> (
      match type_stack with
      | a :: rest -> a :: a :: rest
      | _ ->
          raise
            (TypeError
               ("cannot execute `" ^ token_string
              ^ "`. expected one item on the stack but found none")))
  | UnaryOp Drop | UnaryOp Print | UnaryOp Println -> (
      match type_stack with
      | _ :: rest -> rest
      | _ ->
          raise
            (TypeError
               ("cannot execute `" ^ token_string
              ^ "`. expected one item on the stack but found none")))
  | UnaryOp Swap -> (
      match type_stack with
      | b :: a :: rest -> a :: b :: rest
      | _ ->
          raise
            (TypeError
               ("cannot execute `" ^ token_string
              ^ "`. expected two items on the stack but found one item or none"
               )))
  | UnaryOp Over -> (
      match type_stack with
      | b :: a :: rest -> a :: b :: a :: rest
      | _ ->
          raise
            (TypeError
               ("cannot execute `" ^ token_string
              ^ "`. expected two items on the stack but found one item or none"
               )))
  | UnaryOp Rot -> (
      match type_stack with
      | c :: b :: a :: rest -> a :: c :: b :: rest
      | _ ->
          raise
            (TypeError
               ("cannot execute `" ^ token_string
              ^ "`. expected three items on the stack but found one item, two \
                 items, or none")))
  | BinaryOp Plus
  | BinaryOp Minus
  | BinaryOp Times
  | BinaryOp Divide
  | BinaryOp Exp -> (
      match type_stack with
      | b :: a :: rest ->
          if a = TInt && b = TInt then TInt :: rest
          else
            raise
              (TypeError
                 ("cannot execute `" ^ token_string ^ "`. expected `"
                ^ string_of_token_type TInt ^ "` and `"
                ^ string_of_token_type TInt ^ "` but found `"
                ^ string_of_token_type a ^ "` and `" ^ string_of_token_type b
                ^ "`"))
      | _ ->
          raise
            (TypeError
               ("cannot execute `" ^ token_string
              ^ "`. expected two items of type `" ^ string_of_token_type TInt
              ^ "` on the stack but found one item or none")))
  | BinaryOp Eq
  | BinaryOp Neq
  | BinaryOp Lt
  | BinaryOp Leq
  | BinaryOp Gt
  | BinaryOp Geq -> (
      match type_stack with
      | b :: a :: rest ->
          if a = b then TBool :: rest
          else
            raise
              (TypeError
                 ("cannot execute `" ^ token_string
                ^ "`. expected two items with the same type but found `"
                ^ string_of_token_type a ^ "` and `" ^ string_of_token_type b))
      | _ ->
          raise
            (TypeError
               ("cannot execute `" ^ token_string
              ^ "`. expected two items with the same type on the stack but \
                 found one item or none")))
  | BinaryOp Land | BinaryOp Lor -> (
      match type_stack with
      | b :: a :: rest ->
          if a = TBool && b = TBool then TBool :: rest
          else
            raise
              (TypeError
                 ("cannot execute `" ^ token_string
                ^ "`. expected two items of type `" ^ string_of_token_type TBool
                ^ "` and `" ^ string_of_token_type TBool ^ "` but found `"
                ^ string_of_token_type a ^ "` and `" ^ string_of_token_type b))
      | _ ->
          raise
            (TypeError
               ("cannot execute `" ^ token_string
              ^ "`. expected two items of type `" ^ string_of_token_type TBool
              ^ "` on the stack but found one item or none")))
  | BinaryOp Lnot -> (
      match type_stack with
      | a :: rest ->
          if a = TBool then TBool :: rest
          else
            raise
              (TypeError
                 ("cannot execute `" ^ token_string ^ "`. expected `"
                ^ string_of_token_type TBool ^ "` but found `"
                ^ string_of_token_type a))
      | _ ->
          raise
            (TypeError
               ("cannot execute `" ^ token_string
              ^ "`. expected one item of type `" ^ string_of_token_type TBool
              ^ "` on the stack but found none")))

let type_check (program : program) : unit =
  match program with
  | Program program_tokens ->
      let final = List.fold_left type_check_token [] program_tokens in
      let stack_size = List.length final in
      if stack_size = 0 then ()
      else
        raise
          (TypeError
             (string_of_int (List.length final)
             ^ " unhandled item(s) on the stack"))
