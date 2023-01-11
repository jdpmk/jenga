type 'a environment = (string * 'a) list

let get (env : 'a environment) (s : string) : 'a option =
  let rec aux env q =
    match env with
    | [] -> None
    | (s, x) :: rest -> if s = q then Some x else aux rest q
  in
  aux env s

let put (env : 'a environment) (s : string) (x : 'a) : 'a environment =
  let rec aux env q acc =
    match env with
    | [] -> acc
    | (s_, x_) :: rest ->
        if s_ = q then aux rest q acc else aux rest q ((s_, x_) :: acc)
  in
  let env' = aux env s [] in
  let env'' = (s, x) :: env' in
  env''
