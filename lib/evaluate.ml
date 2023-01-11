open Ast
open Env
open Exceptions

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
    (c : command) =
  let aux c =
    match c with
    | Value _ as v -> v :: stack
    | UnaryOp Dup -> (
        match stack with
        | v :: rest -> v :: v :: rest
        | _ ->
            raise (RuntimeError "unreachable; potential bug in type-checking"))
    | UnaryOp Drop -> (
        match stack with
        | _ :: rest -> rest
        | _ ->
            raise (RuntimeError "unreachable; potential bug in type-checking"))
    | UnaryOp Swap -> (
        match stack with
        | v2 :: v1 :: rest -> v1 :: v2 :: rest
        | _ ->
            raise (RuntimeError "unreachable; potential bug in type-checking"))
    | UnaryOp Over -> (
        match stack with
        | v2 :: v1 :: rest -> v1 :: v2 :: v1 :: rest
        | _ ->
            raise (RuntimeError "unreachable; potential bug in type-checking"))
    | UnaryOp Rot -> (
        match stack with
        | v3 :: v2 :: v1 :: rest -> v1 :: v3 :: v2 :: rest
        | _ ->
            raise (RuntimeError "unreachable; potential bug in type-checking"))
    | UnaryOp Print -> (
        match stack with
        | Value (Primitive (Int i)) :: rest ->
            Printf.printf "%d" i;
            rest
        | Value (Primitive (String s)) :: rest ->
            Printf.printf "%s" s;
            rest
        | Value (Primitive (Bool b)) :: rest ->
            Printf.printf "%B" b;
            rest
        | _ ->
            raise (RuntimeError "unreachable; potential bug in type-checking"))
    | UnaryOp Println -> (
        match stack with
        | Value (Primitive (Int i)) :: rest ->
            Printf.printf "%d\n" i;
            rest
        | Value (Primitive (String s)) :: rest ->
            Printf.printf "%s\n" s;
            rest
        | Value (Primitive (Bool b)) :: rest ->
            Printf.printf "%B\n" b;
            rest
        | _ ->
            raise (RuntimeError "unreachable; potential bug in type-checking"))
    | UnaryOp Eprint -> (
        match stack with
        | Value (Primitive (Int i)) :: rest ->
            Printf.eprintf "%d" i;
            rest
        | Value (Primitive (String s)) :: rest ->
            Printf.eprintf "%s" s;
            rest
        | Value (Primitive (Bool b)) :: rest ->
            Printf.eprintf "%B" b;
            rest
        | _ ->
            raise (RuntimeError "unreachable; potential bug in type-checking"))
    | UnaryOp Eprintln -> (
        match stack with
        | Value (Primitive (Int i)) :: rest ->
            Printf.eprintf "%d\n" i;
            rest
        | Value (Primitive (String s)) :: rest ->
            Printf.eprintf "%s\n" s;
            rest
        | Value (Primitive (Bool b)) :: rest ->
            Printf.eprintf "%B\n" b;
            rest
        | _ ->
            raise (RuntimeError "unreachable; potential bug in type-checking"))
    | BinaryOp Plus -> (
        match stack with
        | Value (Primitive (Int i2)) :: Value (Primitive (Int i1)) :: rest ->
            Value (Primitive (Int (i1 + i2))) :: rest
        | _ ->
            raise (RuntimeError "unreachable; potential bug in type-checking"))
    | BinaryOp Minus -> (
        match stack with
        | Value (Primitive (Int i2)) :: Value (Primitive (Int i1)) :: rest ->
            Value (Primitive (Int (i1 - i2))) :: rest
        | _ ->
            raise (RuntimeError "unreachable; potential bug in type-checking"))
    | BinaryOp Times -> (
        match stack with
        | Value (Primitive (Int i2)) :: Value (Primitive (Int i1)) :: rest ->
            Value (Primitive (Int (i1 * i2))) :: rest
        | _ ->
            raise (RuntimeError "unreachable; potential bug in type-checking"))
    | BinaryOp Divide -> (
        match stack with
        | Value (Primitive (Int i2)) :: Value (Primitive (Int i1)) :: rest ->
            Value (Primitive (Int (i1 / i2))) :: rest
        | _ ->
            raise (RuntimeError "unreachable; potential bug in type-checking"))
    | BinaryOp Mod -> (
        match stack with
        | Value (Primitive (Int i2)) :: Value (Primitive (Int i1)) :: rest ->
            Value (Primitive (Int (i1 mod i2))) :: rest
        | _ ->
            raise (RuntimeError "unreachable; potential bug in type-checking"))
    | BinaryOp Exp -> (
        let tail_rec_int_exp b p =
          if p < 0 then 0
          else
            let rec aux p acc = if p = 0 then acc else b * aux (p - 1) acc in
            aux p 1
        in
        match stack with
        | Value (Primitive (Int i2)) :: Value (Primitive (Int i1)) :: rest ->
            Value (Primitive (Int (tail_rec_int_exp i1 i2))) :: rest
        | _ ->
            raise (RuntimeError "unreachable; potential bug in type-checking"))
    | BinaryOp Eq -> (
        match stack with
        | v2 :: v1 :: rest -> Value (Primitive (Bool (v1 = v2))) :: rest
        | _ ->
            raise (RuntimeError "unreachable; potential bug in type-checking"))
    | BinaryOp Neq -> (
        match stack with
        | v2 :: v1 :: rest -> Value (Primitive (Bool (v1 <> v2))) :: rest
        | _ ->
            raise (RuntimeError "unreachable; potential bug in type-checking"))
    | BinaryOp Lt -> (
        match stack with
        | v2 :: v1 :: rest -> Value (Primitive (Bool (v1 < v2))) :: rest
        | _ ->
            raise (RuntimeError "unreachable; potential bug in type-checking"))
    | BinaryOp Leq -> (
        match stack with
        | v2 :: v1 :: rest -> Value (Primitive (Bool (v1 <= v2))) :: rest
        | _ ->
            raise (RuntimeError "unreachable; potential bug in type-checking"))
    | BinaryOp Gt -> (
        match stack with
        | v2 :: v1 :: rest -> Value (Primitive (Bool (v1 > v2))) :: rest
        | _ ->
            raise (RuntimeError "unreachable; potential bug in type-checking"))
    | BinaryOp Geq -> (
        match stack with
        | v2 :: v1 :: rest -> Value (Primitive (Bool (v1 >= v2))) :: rest
        | _ ->
            raise (RuntimeError "unreachable; potential bug in type-checking"))
    | BinaryOp Land -> (
        match stack with
        | Value (Primitive (Bool b2)) :: Value (Primitive (Bool b1)) :: rest ->
            Value (Primitive (Bool (b1 && b2))) :: rest
        | _ ->
            raise (RuntimeError "unreachable; potential bug in type-checking"))
    | BinaryOp Lor -> (
        match stack with
        | Value (Primitive (Bool b2)) :: Value (Primitive (Bool b1)) :: rest ->
            Value (Primitive (Bool (b1 || b2))) :: rest
        | _ ->
            raise (RuntimeError "unreachable; potential bug in type-checking"))
    | BinaryOp Lnot -> (
        match stack with
        | Value (Primitive (Bool b)) :: rest ->
            Value (Primitive (Bool (not b))) :: rest
        | _ ->
            raise (RuntimeError "unreachable; potential bug in type-checking"))
    | IfElse (condition, if_body, else_body) -> (
        match eval_block m condition stack with
        | _, Value (Primitive (Bool b)) :: rest ->
            let _, stack =
              if b then eval_block m if_body rest
              else eval_block m else_body rest
            in
            stack
        | _ ->
            raise (RuntimeError "unreachable; potential bug in type-checking"))
    | While (condition, body) -> (
        match eval_block m condition stack with
        | _, Value (Primitive (Bool b)) :: rest ->
            if b then
              let _, stack = eval_command (eval_block m body rest) c in
              stack
            else rest
        | _ ->
            raise (RuntimeError "unreachable; potential bug in type-checking"))
    | MemoryOp _ -> raise (Failure "unimplemented")
  in
  (m, aux c)

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
