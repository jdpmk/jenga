open Ast
open Env
open Exceptions

let unreachable = "unreachable; potential bug in type-checking"

let build_memory (m : memory) : value environment =
  match m with
  | Memory allocations ->
      List.fold_left
        (fun env (Alloc (i, t, init)) ->
          let allocated =
            match t with
            | TPrimitive TInt -> Primitive init
            | TPrimitive TChar -> Primitive init
            | TPrimitive TString -> Primitive init
            | TPrimitive TBool -> Primitive init
            | TCompound (TArr (t, size)) ->
                Compound (Array (t, Array.make size init, size))
            | _ -> raise (Failure "unreachable")
          in
          put env i allocated)
        [] allocations

let rec eval_command ((m, stack) : value environment * command list)
    (c : command) : value environment * command list =
  match c with
  | Value _ as v -> (m, v :: stack)
  | UnaryOp Dup -> (
      match stack with
      | v :: rest -> (m, v :: v :: rest)
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | UnaryOp Drop -> (
      match stack with
      | _ :: rest -> (m, rest)
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | UnaryOp Swap -> (
      match stack with
      | v2 :: v1 :: rest -> (m, v1 :: v2 :: rest)
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | UnaryOp Over -> (
      match stack with
      | v2 :: v1 :: rest -> (m, v1 :: v2 :: v1 :: rest)
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | UnaryOp Rot -> (
      match stack with
      | v3 :: v2 :: v1 :: rest -> (m, v1 :: v3 :: v2 :: rest)
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | UnaryOp Print -> (
      match stack with
      | Value (Primitive (Int i)) :: rest ->
          Printf.printf "%d" i;
          (m, rest)
      | Value (Primitive (String s)) :: rest ->
          Printf.printf "%s" s;
          (m, rest)
      | Value (Primitive (Bool b)) :: rest ->
          Printf.printf "%B" b;
          (m, rest)
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | UnaryOp Println -> (
      match stack with
      | Value (Primitive (Int i)) :: rest ->
          Printf.printf "%d\n" i;
          (m, rest)
      | Value (Primitive (String s)) :: rest ->
          Printf.printf "%s\n" s;
          (m, rest)
      | Value (Primitive (Bool b)) :: rest ->
          Printf.printf "%B\n" b;
          (m, rest)
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | UnaryOp Eprint -> (
      match stack with
      | Value (Primitive (Int i)) :: rest ->
          Printf.eprintf "%d" i;
          (m, rest)
      | Value (Primitive (String s)) :: rest ->
          Printf.eprintf "%s" s;
          (m, rest)
      | Value (Primitive (Bool b)) :: rest ->
          Printf.eprintf "%B" b;
          (m, rest)
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | UnaryOp Eprintln -> (
      match stack with
      | Value (Primitive (Int i)) :: rest ->
          Printf.eprintf "%d\n" i;
          (m, rest)
      | Value (Primitive (String s)) :: rest ->
          Printf.eprintf "%s\n" s;
          (m, rest)
      | Value (Primitive (Bool b)) :: rest ->
          Printf.eprintf "%B\n" b;
          (m, rest)
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Plus -> (
      match stack with
      | Value (Primitive (Int i2)) :: Value (Primitive (Int i1)) :: rest ->
          (m, Value (Primitive (Int (i1 + i2))) :: rest)
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Minus -> (
      match stack with
      | Value (Primitive (Int i2)) :: Value (Primitive (Int i1)) :: rest ->
          (m, Value (Primitive (Int (i1 - i2))) :: rest)
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Times -> (
      match stack with
      | Value (Primitive (Int i2)) :: Value (Primitive (Int i1)) :: rest ->
          (m, Value (Primitive (Int (i1 * i2))) :: rest)
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Divide -> (
      match stack with
      | Value (Primitive (Int i2)) :: Value (Primitive (Int i1)) :: rest ->
          (m, Value (Primitive (Int (i1 / i2))) :: rest)
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Mod -> (
      match stack with
      | Value (Primitive (Int i2)) :: Value (Primitive (Int i1)) :: rest ->
          (m, Value (Primitive (Int (i1 mod i2))) :: rest)
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Exp -> (
      let tail_rec_int_exp b p =
        if p < 0 then 0
        else
          let rec aux p acc = if p = 0 then acc else b * aux (p - 1) acc in
          aux p 1
      in
      match stack with
      | Value (Primitive (Int i2)) :: Value (Primitive (Int i1)) :: rest ->
          (m, Value (Primitive (Int (tail_rec_int_exp i1 i2))) :: rest)
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Eq -> (
      match stack with
      | v2 :: v1 :: rest -> (m, Value (Primitive (Bool (v1 = v2))) :: rest)
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Neq -> (
      match stack with
      | v2 :: v1 :: rest -> (m, Value (Primitive (Bool (v1 <> v2))) :: rest)
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Lt -> (
      match stack with
      | v2 :: v1 :: rest -> (m, Value (Primitive (Bool (v1 < v2))) :: rest)
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Leq -> (
      match stack with
      | v2 :: v1 :: rest -> (m, Value (Primitive (Bool (v1 <= v2))) :: rest)
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Gt -> (
      match stack with
      | v2 :: v1 :: rest -> (m, Value (Primitive (Bool (v1 > v2))) :: rest)
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Geq -> (
      match stack with
      | v2 :: v1 :: rest -> (m, Value (Primitive (Bool (v1 >= v2))) :: rest)
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Land -> (
      match stack with
      | Value (Primitive (Bool b2)) :: Value (Primitive (Bool b1)) :: rest ->
          (m, Value (Primitive (Bool (b1 && b2))) :: rest)
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Lor -> (
      match stack with
      | Value (Primitive (Bool b2)) :: Value (Primitive (Bool b1)) :: rest ->
          (m, Value (Primitive (Bool (b1 || b2))) :: rest)
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | BinaryOp Lnot -> (
      match stack with
      | Value (Primitive (Bool b)) :: rest ->
          (m, Value (Primitive (Bool (not b))) :: rest)
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | IfElse (condition, if_body, else_body) -> (
      match eval_block m condition stack with
      | m', Value (Primitive (Bool b)) :: rest ->
          if b then eval_block m' if_body rest else eval_block m' else_body rest
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | While (condition, body) -> (
      match eval_block m condition stack with
      | m', Value (Primitive (Bool b)) :: rest ->
          if b then eval_command (eval_block m' body rest) c else (m', rest)
      | _ -> raise (RuntimeError "unreachable; potential bug in type-checking"))
  | MemoryOp Read -> (
      match stack with
      | Value (Primitive (Int i)) :: Value (Primitive (Identifier id)) :: rest
        -> (
          match get m id with
          | Some (Compound (Array (_, a, _))) ->
              (m, Value (Primitive a.(i)) :: rest)
          | _ -> raise (RuntimeError unreachable))
      | Value (Primitive (Identifier id)) :: rest -> (
          match get m id with
          | Some (Primitive _ as v) -> (m, Value v :: rest)
          | _ -> raise (RuntimeError unreachable))
      | _ -> raise (RuntimeError unreachable))
  | MemoryOp Write -> (
      match stack with
      | Value (Primitive (_ as v))
        :: Value (Primitive (Int i))
        :: Value (Primitive (Identifier id))
        :: rest -> (
          match get m id with
          | Some (Compound (Array (t, a, size))) ->
              a.(i) <- v;
              let m' = put m id (Compound (Array (t, a, size))) in
              (m', rest)
          | _ -> raise (RuntimeError unreachable))
      | Value (Primitive _ as v) :: Value (Primitive (Identifier id)) :: rest ->
          let m' = put m id v in
          (m', rest)
      | _ -> raise (RuntimeError unreachable))

and eval_block (m : value environment) (b : block)
    (initial_stack : command list) : value environment * command list =
  match b with
  | Block commands -> List.fold_left eval_command (m, initial_stack) commands

let eval_program (p : program) : unit =
  match p with
  | Program (m, b) ->
      let m = build_memory m in
      let _ = eval_block m b [] in
      ()
