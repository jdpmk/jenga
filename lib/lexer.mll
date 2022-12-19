{
  open Parser
}

(* TODO: add more rules to parse tokens *)
rule token = parse
  | "dup" { DUP }
