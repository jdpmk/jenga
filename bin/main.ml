open Jenga.Exceptions
open Jenga.Lexer
open Jenga.Parser
open Jenga.Types

let print_error_with_loc message lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  let filename = pos.pos_fname in
  let line = pos.pos_lnum in
  let column = pos.pos_cnum - pos.pos_bol + 1 in
  Printf.printf "error: %s:%d:%d %s" filename line column message

let () =
  (* TODO: read program from file provided via command line args *)
  let example_program = "1 2 + println" in
  let program_code = example_program in
  let lexbuf = Lexing.from_string program_code in
  let program =
    try parse_program read_token lexbuf
    with SyntaxError message ->
      print_error_with_loc message lexbuf;
      exit 1
  in
  type_check program
