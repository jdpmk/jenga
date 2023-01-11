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
           assert_equal (Program (Block [])) (lex_and_parse "") );
         ( "single token" >:: fun _ ->
           assert_equal (Program (Block [ UnaryOp Dup ])) (lex_and_parse "dup")
         );
         ( "mix of tokens" >:: fun _ ->
           assert_equal
             (Program
                (Block
                   [
                     UnaryOp Dup;
                     UnaryOp Drop;
                     UnaryOp Swap;
                     Value (Int 1);
                     Value (String "a");
                     Value (Bool false);
                   ]))
             (lex_and_parse "dup drop swap 1 \"a\" false") );
         ( "simple program with a comment" >:: fun _ ->
           assert_equal
             (Program
                (Block
                   [
                     Value (Int 1);
                     Value (Int 2);
                     BinaryOp Plus;
                     UnaryOp Println;
                   ]))
             (lex_and_parse "# this is a comment\n1 2 + println") );
         ( "program with ifelse" >:: fun _ ->
           assert_equal
             (Program
                (Block
                   [
                     Value (Int 0);
                     IfElse
                       ( Block [ UnaryOp Dup; Value (Int 1); BinaryOp Eq ],
                         Block [ Value (String "equal"); UnaryOp Println ],
                         Block [ Value (String "not equal"); UnaryOp Println ]
                       );
                   ]))
             (lex_and_parse
                "0 if dup 1 = then \"equal\" println else \"not equal\" \
                 println end") );
         ( "program with while" >:: fun _ ->
           assert_equal
             (Program
                (Block
                   [
                     Value (Int 0);
                     While
                       ( Block [ UnaryOp Dup; Value (Int 5); BinaryOp Neq ],
                         Block
                           [
                             UnaryOp Dup;
                             UnaryOp Print;
                             Value (String " ");
                             UnaryOp Print;
                             Value (Int 1);
                             BinaryOp Plus;
                           ] );
                     Value (String "\n");
                     UnaryOp Print;
                   ]))
             (lex_and_parse
                "0 while dup 5 != do dup print \" \" print 1 + end \"\n\" print")
         );
       ]

let () = run_test_tt_main tests
