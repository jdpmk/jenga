type 'a environment = (string * 'a) list

let get (env : 'a environment) (s : string) : 'a option =
  let rec aux env q =
    match env with
    | [] -> None
    | (s, x) :: rest -> if s = q then Some x else aux rest q
  in
  aux env s

let put (env : 'a environment) (s : string) (x : 'a) : 'a environment =
  (s, x) :: env
