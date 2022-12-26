open Ast
open Exceptions

let rec eval_command (stack : command list) (c : command) =
  match c with
  | Const _ as v -> v :: stack
  | UnaryOp Dup -> (
      match stack with
      | v :: rest -> v :: v :: rest
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | UnaryOp Drop -> (
      match stack with
      | _ :: rest -> rest
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | UnaryOp Swap -> (
      match stack with
      | v2 :: v1 :: rest -> v1 :: v2 :: rest
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | UnaryOp Over -> (
      match stack with
      | v2 :: v1 :: rest -> v1 :: v2 :: v1 :: rest
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | UnaryOp Rot -> (
      match stack with
      | v3 :: v2 :: v1 :: rest -> v1 :: v3 :: v2 :: rest
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | UnaryOp Print -> (
      match stack with
      | Const (Int i) :: rest ->
          Printf.printf "%d" i;
          rest
      | Const (String s) :: rest ->
          Printf.printf "%s" s;
          rest
      | Const (Bool b) :: rest ->
          Printf.printf "%B" b;
          rest
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | UnaryOp Println -> (
      match stack with
      | Const (Int i) :: rest ->
          Printf.printf "%d\n" i;
          rest
      | Const (String s) :: rest ->
          Printf.printf "%s\n" s;
          rest
      | Const (Bool b) :: rest ->
          Printf.printf "%B\n" b;
          rest
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Plus -> (
      match stack with
      | Const (Int i2) :: Const (Int i1) :: rest ->
          Const (Int (i1 + i2)) :: rest
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Minus -> (
      match stack with
      | Const (Int i2) :: Const (Int i1) :: rest ->
          Const (Int (i1 - i2)) :: rest
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Times -> (
      match stack with
      | Const (Int i2) :: Const (Int i1) :: rest ->
          Const (Int (i1 * i2)) :: rest
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Divide -> (
      match stack with
      | Const (Int i2) :: Const (Int i1) :: rest ->
          Const (Int (i1 / i2)) :: rest
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Exp -> (
      let tail_rec_int_exp b p =
        if p < 0 then 0
        else
          let rec aux p acc = if p = 0 then acc else b * aux (p - 1) acc in
          aux p 1
      in
      match stack with
      | Const (Int i2) :: Const (Int i1) :: rest ->
          Const (Int (tail_rec_int_exp i1 i2)) :: rest
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Eq -> (
      match stack with
      | v2 :: v1 :: rest -> Const (Bool (v1 = v2)) :: rest
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Neq -> (
      match stack with
      | v2 :: v1 :: rest -> Const (Bool (v1 <> v2)) :: rest
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Lt -> (
      match stack with
      | v2 :: v1 :: rest -> Const (Bool (v1 < v2)) :: rest
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Leq -> (
      match stack with
      | v2 :: v1 :: rest -> Const (Bool (v1 <= v2)) :: rest
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Gt -> (
      match stack with
      | v2 :: v1 :: rest -> Const (Bool (v1 > v2)) :: rest
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Geq -> (
      match stack with
      | v2 :: v1 :: rest -> Const (Bool (v1 >= v2)) :: rest
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Land -> (
      match stack with
      | Const (Bool b2) :: Const (Bool b1) :: rest ->
          Const (Bool (b1 && b2)) :: rest
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Lor -> (
      match stack with
      | Const (Bool b2) :: Const (Bool b1) :: rest ->
          Const (Bool (b1 || b2)) :: rest
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Lnot -> (
      match stack with
      | Const (Bool b) :: rest -> Const (Bool (not b)) :: rest
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | IfElse (condition, if_body, else_body) -> (
      match eval_block condition stack with
      | Const (Bool b) :: rest ->
          if b then eval_block if_body rest else eval_block else_body rest
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | While (condition, body) -> (
      match eval_block condition stack with
      | Const (Bool b) :: rest ->
          if b then eval_command (eval_block body rest) c else rest
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))

and eval_block (b : block) (initial_stack : command list) : command list =
  match b with
  | Block commands -> List.fold_left eval_command initial_stack commands

let eval_program (p : program) : unit =
  match p with
  | Program b ->
      let _ = eval_block b [] in
      ()
