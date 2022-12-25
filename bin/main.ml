open Sys
open Jenga.Exceptions
open Jenga.Lexer
open Jenga.Parser
open Jenga.Types

let print_usage args = Printf.fprintf stderr "usage: %s program.jg" args.(0)

let print_error_with_loc message lexbuf =
  let pos = lexbuf.Lexing.lex_curr_p in
  let line = pos.pos_lnum in
  let column = pos.pos_cnum - pos.pos_bol + 1 in
  Printf.fprintf stderr "error: %d:%d %s" line column message

let () =
  if Array.length argv <> 2 then print_usage argv
  else
    let filepath = argv.(1) in
    let ic = open_in filepath in
    let lexbuf = Lexing.from_channel ~with_positions:true ic in
    let program =
      try parse_program read_token lexbuf
      with SyntaxError message ->
        print_error_with_loc message lexbuf;
        exit 1
    in
    type_check_program program
    (* TODO: implement program evaluation *)
