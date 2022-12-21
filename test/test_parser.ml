open OUnit2
open Jenga.Ast
open Jenga.Lexer
open Jenga.Parser

let lex_and_parse (s : string) : program =
  let lexbuf = Lexing.from_string s in
  let ast = parse_program read_token lexbuf in
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
         ( "simple program with a comment" >:: fun _ ->
           assert_equal
             (Program
                [ Const (Int 1); Const (Int 2); BinaryOp Plus; UnaryOp Println ])
             (lex_and_parse "# this is a comment\n1 2 + println") );
       ]

let () = run_test_tt_main tests
