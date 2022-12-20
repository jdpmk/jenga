open OUnit2
open Jenga.Ast
open Jenga.Lexer
open Jenga.Parser

let lex_and_parse (s : string) : program =
  let lexbuf = Lexing.from_string s in
  let ast = main read_token lexbuf in
  ast

let tests =
  "parser tests"
  >::: [
         ( "empty program" >:: fun _ ->
           assert_equal (Program []) (lex_and_parse "") );
         ( "single token" >:: fun _ ->
           assert_equal (Program [ UnaryOp Dup ]) (lex_and_parse "dup") );
         ( "mix of tokens" >:: fun _ ->
           assert_equal
             (Program
                [
                  UnaryOp Dup;
                  UnaryOp Drop;
                  UnaryOp Swap;
                  Const (Int 1);
                  Const (String "a");
                  Const (Bool false);
                ])
             (lex_and_parse "dup drop swap 1 \"a\" false") );
       ]

let () = run_test_tt_main tests
